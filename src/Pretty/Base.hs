{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE TypeSynonymInstances               #-}
{-# LANGUAGE FlexibleInstances                  #-}

module Pretty.Base where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text (Text)

import qualified Data.Loc as Loc

import Data.Loc
import qualified Data.Set as Set
import Data.Monoid (mempty, (<>))
import System.IO


--------------------------------------------------------------------------------
-- | Report with colour and source

class Report a where
  report :: a -> Doc AnsiStyle
  report a = reportS a Nothing

  reportS :: a -> Maybe ByteString -> Doc AnsiStyle
  reportS a _ = report a

data ReportMsg
  = H1 Text       -- appears red
  | P (Doc AnsiStyle)
  | CODE Loc
  deriving (Show)

instance Report ReportMsg where
  reportS (H1 s) _ = annotate (color Red) $ pretty s
  reportS (P s) _ = s <> line
  reportS (CODE loc) Nothing = annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
  reportS (CODE loc) (Just src) = indent 2 $ vsep
        [ annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
        , reAnnotate toAnsiStyle $ prettySourceCode $ SourceCode src loc 1
        ]

instance Report [ReportMsg] where
  reportS msgs src = vsep $
          [ softline' ]
      ++  map (\msg -> indent 2 $ reportS msg src <> line) msgs
      ++  [ softline' ]


--------------------------------------------------------------------------------
-- | Source Code Annotation

data SourceCodeAnnotation
  = Other
  | HighlightedLineNo
  | HighlightedArea

data SourceCode = SourceCode
                    ByteString  -- source code
                    Loc         -- highlighted location
                    Int         -- number of the neighboring lines to be rendered

printSourceCode :: SourceCode -> IO ()
printSourceCode = renderIO stdout . reAnnotateS toAnsiStyle . layoutPretty defaultLayoutOptions . prettySourceCode

toAnsiStyle :: SourceCodeAnnotation -> AnsiStyle
toAnsiStyle Other = mempty
toAnsiStyle HighlightedLineNo = colorDull Blue
toAnsiStyle HighlightedArea = color Red

-- instance Pretty SourceCode where
prettySourceCode :: SourceCode -> Doc SourceCodeAnnotation
prettySourceCode (SourceCode source NoLoc _) = pretty $ BS.unpack source
prettySourceCode (SourceCode source (Loc from to) spread) =
  vsep $ zipWith (<+>) lineNos lines'
      ++ [softline']

  where   sourceLines = lines (BS.unpack source)

          start = (posLine from - spread) `max` 1
          end   = (posLine to   + spread) `min` length sourceLines

          -- max width of the greatest line number
          lineNoColumnWidth = 2 * ceiling (fromIntegral (lineNoWidth end) / 2.0 :: Double)

          -- measures the width of a number (decimal)
          lineNoWidth :: Int -> Int
          lineNoWidth = succ . (floor :: Double -> Int) . logBase 10 . fromIntegral

          prettyLineNo :: Int -> Doc SourceCodeAnnotation
          prettyLineNo n =
                pretty (replicate (lineNoColumnWidth - lineNoWidth n) ' ')
            <>  pretty n
            <+> pretty '|'

          lineNos :: [Doc SourceCodeAnnotation]
          lineNos =
                [                              prettyLineNo n | n <- [ start          .. posLine from - 1 ]  ]
            ++  [ annotate HighlightedLineNo $ prettyLineNo n | n <- [ posLine from   .. posLine to       ]  ]
            ++  [                              prettyLineNo n | n <- [ posLine to + 1 .. end                  ]  ]


          lines' :: [Doc SourceCodeAnnotation]
          lines' = map prettyLine $ zip [ start .. end ]
            $ drop (start - 1)
            $ take end
            $ sourceLines

          substring :: Int -> Maybe Int -> String -> String
          substring startFrom Nothing       = drop (startFrom - 1)
          substring startFrom (Just endAt)  = drop (startFrom - 1) . take endAt

          prettyLine :: (Int, String) -> Doc SourceCodeAnnotation
          prettyLine (n, s)
            | n == posLine from && n == posLine to =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) s)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  (Just (posCol to))       s)
                <>  annotate Other            (pretty $ substring (posCol to + 1) Nothing                 s)
            | n == posLine from =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) s)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  Nothing                  s)
            | n == posLine to =
                    annotate HighlightedArea  (pretty $ substring 0              (Just (posCol to))       s)
                <>  annotate Other            (pretty $ substring (posCol to)    Nothing                  s)
            | n > posLine from && n < posLine to =
                    annotate HighlightedArea  (pretty s)
            | otherwise =
                    annotate Other            (pretty s)


--------------------------------------------------------------------------------
-- | Instances

instance Pretty a => Pretty (Set.Set a) where
  pretty s = encloseSep "{" "}" ", " (map pretty $ Set.toList s)

-- instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
--   pretty m = encloseSep "{" "}" ", " (map (\(k, v) -> pretty k <+> ":" <+> pretty v) $ Map.toList m)


--------------------------------------------------------------------------------
-- | Precedences

class PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

-- adds parentheses on True
parensIf :: Bool -> Doc ann -> Doc ann
parensIf False xs = xs
parensIf True  xs = parens xs
