module Syntax.Parser
  ( parseProgram
  , parseProcess
  -- , parseConcreteProcess
  -- , parseAbstractProcess
  -- , parseAbstraceSessionSyntax
  -- , parseConcreteProgram
  , ParseError(..)
  )
  where

import Syntax.Base
import Syntax.Concrete
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



parseProcess :: ByteString -> Either ParseError Process
parseProcess src = runExcept (evalStateT processParser initState)
  where filePath = "<interactive>"
        initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src)) src
        startingLoc = Loc (startPos filePath) (startPos filePath)


-- parseConcreteProcess :: ByteString -> Either ParseError Process
-- parseConcreteProcess src = runExcept (evalStateT processParser initState)
--   where filePath = "<interactive>"
--         initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src)) src
--         startingLoc = Loc (startPos filePath) (startPos filePath)
--
-- parseAbstractProcess :: ByteString -> Either ParseError B.Process
-- parseAbstractProcess src = bind <$> parseConcreteProcess src

-- parseAbstraceSessionSyntax :: ByteString -> Either ParseError B.SessionSyntax
-- parseAbstraceSessionSyntax src = bind <$> runExcept (evalStateT sessionSyntaxParser initState)
--   where filePath = "<interactive>"
--         initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src)) src
--         startingLoc = Loc (startPos filePath) (startPos filePath)

parseProgram :: FilePath -> ByteString -> Either ParseError Program
parseProgram filePath src = runExcept (evalStateT programParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src)) src
        startingLoc = Loc (startPos filePath) (startPos filePath)

-- parseProgram :: FilePath -> ByteString -> Either ParseError B.Program
-- parseProgram filePath src = bind <$> parseConcreteProgram filePath src
