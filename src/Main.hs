module Main where

import Syntax.Abstract
import Syntax.Parser
import TypeChecker

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BC


main :: IO ()
main = do
  let filePath = "test/source/a.clp"
  readResult <- BS.readFile filePath
  BC.putStrLn readResult

  let parseResult = parseProgram filePath readResult
  case parseResult of
    Left err -> print (err :: ParseError)
    Right (Program declarations) -> do
      let TermDecl _ term = head declarations
      -- print (infer emptyEnv term)
      print term
