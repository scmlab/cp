{-# LANGUAGE OverloadedStrings                  #-}

module Pretty where

import Pretty.Syntax.Abstract ()
import Pretty.Syntax.Concrete ()

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Loc hiding (Pos)
import qualified Data.Set as Set
import Data.Monoid (mempty, (<>))
import Data.Text.Prettyprint.Doc hiding (line)
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.IO

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
          prettyLine (n, line)
            | n == posLine from && n == posLine to =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) line)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  (Just (posCol to))       line)
                <>  annotate Other            (pretty $ substring (posCol to + 1) Nothing                 line)
            | n == posLine from =
                    annotate Other            (pretty $ substring 0              (Just (posCol from - 1)) line)
                <>  annotate HighlightedArea  (pretty $ substring (posCol from)  Nothing                  line)
            | n == posLine to =
                    annotate HighlightedArea  (pretty $ substring 0              (Just (posCol to))       line)
                <>  annotate Other            (pretty $ substring (posCol to)    Nothing                  line)
            | n > posLine from && n < posLine to =
                    annotate HighlightedArea  (pretty line)
            | otherwise =
                    annotate Other            (pretty line)


--------------------------------------------------------------------------------
-- | Instances

instance Pretty a => Pretty (Set.Set a) where
  pretty s = encloseSep "{" "}" ", " (map pretty $ Set.toList s)
