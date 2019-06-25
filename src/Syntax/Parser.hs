module Syntax.Parser
  ( parseProgram
  , parseConcreteProcess
  , parseAbstractProcess
  , parseAbstraceSessionSyntax
  , parseConcreteProgram
  , ParseError(..)
  )
  where

import Syntax.Base
import qualified Syntax.Binding as B
-- import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Parser.Parser (programParser, processParser, sessionSyntaxParser)
import Syntax.Parser.Lexer (lexer)
import Syntax.Parser.Type
import TypeChecking.Binding

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Loc
import Language.Lexer.Applicative

parseConcreteProcess :: ByteString -> Either ParseError C.Process
parseConcreteProcess src = runExcept (evalStateT processParser initState)
  where filePath = "<interactive>"
        initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src)) src
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseAbstractProcess :: ByteString -> Either ParseError B.Process
parseAbstractProcess src = toBinding <$> parseConcreteProcess src

parseAbstraceSessionSyntax :: ByteString -> Either ParseError B.SessionSyntax
parseAbstraceSessionSyntax src = toBinding <$> runExcept (evalStateT sessionSyntaxParser initState)
  where filePath = "<interactive>"
        initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src)) src
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseConcreteProgram :: FilePath -> ByteString -> Either ParseError C.Program
parseConcreteProgram filePath src = runExcept (evalStateT programParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src)) src
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseProgram :: FilePath -> ByteString -> Either ParseError B.Program
parseProgram filePath src = toBinding <$> parseConcreteProgram filePath src
