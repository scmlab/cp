module Syntax.Parser
  ( parseProgram
  , parseConcreteProgram
  , ParseError(..)
  )
  where

import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Parser.Parser (programParser)
import Syntax.Parser.Lexer (lexer)
import Syntax.Parser.Type

import Control.Monad.Except
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Loc
import Language.Lexer.Applicative

-- parseProcess :: ByteString -> Either ParseError Pi
-- parseProcess src = fromConcrete <$> runExcept (evalStateT processParser initState)
--   where filePath = ""
--         initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
--         startingLoc = Loc (startPos filePath) (startPos  filePath)

parseConcreteProgram :: FilePath -> ByteString -> Either ParseError (C.Program Loc)
parseConcreteProgram filePath src = runExcept (evalStateT programParser initState)
  where initState = ParserState startingLoc startingLoc (runLexer lexer filePath (BS.unpack src))
        startingLoc = Loc (startPos filePath) (startPos filePath)

parseProgram :: FilePath -> ByteString -> Either ParseError A.Program
parseProgram filePath src = C.toAbstract <$> parseConcreteProgram filePath src
