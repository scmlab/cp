{-# LANGUAGE OverloadedStrings                  #-}
module Main where

import qualified Syntax.Concrete as C
import Syntax.Parser
import TypeChecking
import TypeChecking.Types
import Pretty.Error
import Base

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Loc (Loc(..))
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- import qualified Data.ByteString.Lazy.Char8 as BC

import Control.Exception (IOException, try)
import Control.Monad.State
import Control.Monad.Except

putSource :: Maybe ByteString -> M ()
putSource x = modify $ \ st -> st { stSource = x }

getSourceOrThrow :: M ByteString
getSourceOrThrow = do
  result <- gets stSource
  case result of
    Nothing -> throwError $ Panic "Can't read the stored source code"
    Just source -> return source

--------------------------------------------------------------------------------
-- |

readSource :: FilePath -> M ByteString
readSource filePath = do
    readResult <- liftIO $ try (BS.readFile filePath)
    case readResult of
        Left err -> throwError $ Panic $ show (err :: IOException)
        Right source -> do
          -- store the source in the monad for later debugging use
          putSource (Just source)
          return source

parseSource :: FilePath -> ByteString -> M (C.Program Loc)
parseSource filePath source = do
    case parseConcreteProgram filePath source of
        Left err -> throwError $ ParseError err
        Right v -> return v

handleError :: M () -> M ()
handleError program = do
  source <- getSourceOrThrow

  program `catchError` (\err -> liftIO $ putDoc $ prettyError err source)


runTCM :: TCM a -> M (a, TCState)
runTCM program = do
    let (result, s) = runState (runExceptT program) initialTCState
    case result of
      Left err -> throwError $ TypeError err
      Right val -> return (val, s)

main :: IO ()
main = void $ runM $ handleError $ do
    let filePath = "test/source/buy-sell.clp"
    program <- readSource filePath >>= parseSource filePath

    (inferred, _) <- runTCM (checkAll program)

    -- error $ show inferred

    -- printing inferred sessions
    _ <- Map.traverseWithKey (\name session -> do
      liftIO $ putStrLn $ show $ pretty name
      liftIO $ putStrLn $ show $ pretty session) inferred





    return ()
