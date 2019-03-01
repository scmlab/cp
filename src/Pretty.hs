module Pretty where


import Syntax.Abstract

import Data.Loc hiding (Pos)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc hiding (line)
import System.Console.ANSI
import System.IO


--------------------------------------------------------------------------------
-- | Source Code Annotation

data SourceCodeAnnotation
  = Other
  | HighlightedLineNo
  | HighlightedArea

data SourceCode = SourceCode
                    String    -- source code
                    Loc       -- highlighted location
                    Int       -- number of the neighboring lines to be rendered

printSourceCode :: SourceCode -> IO ()
printSourceCode = printAnnotation . layoutPretty defaultLayoutOptions . prettySourceCode


printAnnotation :: SimpleDocStream SourceCodeAnnotation -> IO ()
printAnnotation x = case x of
  SFail -> error "panic: failed to render annotated source code"
  SEmpty -> do
    hFlush stdout
    return ()
  SChar c xs -> do
    putChar c
    printAnnotation xs
  SText _ t xs -> do
    Text.putStr t
    printAnnotation xs
  SLine i xs -> do
    putStr ('\n' : replicate i ' ')
    printAnnotation xs
  SAnnPush code xs -> do
    setSGR (translateSourceCodeAnnotation code)
    printAnnotation xs
  SAnnPop xs -> do
    setSGR []
    printAnnotation xs

translateSourceCodeAnnotation :: SourceCodeAnnotation -> [SGR]
translateSourceCodeAnnotation Other             = [Reset]
translateSourceCodeAnnotation HighlightedLineNo = [SetColor Foreground Dull Blue]
translateSourceCodeAnnotation HighlightedArea   = [SetColor Foreground Vivid Red]

-- instance Pretty SourceCode where
prettySourceCode :: SourceCode -> Doc SourceCodeAnnotation
prettySourceCode (SourceCode source NoLoc _) = pretty source
prettySourceCode (SourceCode source (Loc from to) spread) =
  vsep $  [softline']
      ++  zipWith (<+>) lineNos lines'
      ++  [softline', softline']

  where   sourceLines = lines source

          start = (posLine from - spread) `max` 1
          end   = (posLine to   + spread) `min` length sourceLines

          -- max width of the greatest line number
          lineNoColumnWidth = 4 * ceiling (fromIntegral (lineNoWidth end) / 4.0 :: Double)

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
