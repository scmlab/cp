module Main where

import Syntax.Abstract
import Syntax.Parser
import TypeChecker

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BC

-- import Control.IOException (IOException)

import Control.Exception (IOException, Exception, try)
import Control.Monad.State
import Control.Monad.Except

data MState = MState
  { getSource :: Maybe ByteString
  } deriving (Show)

data Error = ParseError ParseError
           | OtherError String
           deriving (Show)

type M = ExceptT Error (StateT MState IO)

runM :: M a -> IO (Either Error a, MState)
runM program = runStateT (runExceptT program) initialState
    where
        initialState = MState Nothing


readSource :: FilePath -> M ByteString
readSource filePath = do
    readResult <- liftIO $ try (BS.readFile filePath)
    case readResult of
        Left err -> throwError $ OtherError $ show (err :: IOException)
        Right source -> do
          -- store the source in the monad for later debugging use
          put (MState (Just source))
          return source

parseSource :: FilePath -> ByteString -> M Program
parseSource filePath source = do
    case parseProgram filePath source of
        Left err -> throwError $ ParseError err
        Right v -> return v

handleError :: M () -> M ()
handleError program = program `catchError` \ err -> case err of
  ParseError parseError -> gets getSource >>= liftIO . printParseError parseError
  OtherError msg -> liftIO (putStrLn (show msg))

main :: IO ()
main = void $ runM $ handleError $ do
    let filePath = "test/source/a.clp"
    program <- readSource filePath >>= parseSource filePath
    liftIO $ print program
    return ()
