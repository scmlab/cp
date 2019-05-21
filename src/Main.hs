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
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Render.Terminal

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)

-- import qualified Data.ByteString.Lazy.Char8 as BC

import Control.Exception (IOException, try)
import Control.Monad.State
import Control.Monad.Except

import System.Console.Haskeline
import System.Console.GetOpt
import System.Environment

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
main = do
  (opts, _filePaths) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> do
          -- outputStrLn $ show $ parseCommand input
          liftIO $ handleCommand $ parseCommand input
          loop
    --
    --
    -- void $ runM $ handleError $ do
    -- let filePath = "test/source/church.clp"
    -- program <- readSource filePath >>= parseSource filePath
    --
    -- (inferred, _) <- runTCM (checkAll program)
    --
    -- -- printing inferred sessions
    -- _ <- Map.traverseWithKey (\name session -> do
    --   liftIO $ putStrLn $ show $ pretty name
    --   liftIO $ putStrLn $ show $ pretty session) inferred
    --
    --
    --
    --
    --
    -- return ()

--------------------------------------------------------------------------------
-- | Command-line arguments

data Mode = ModeREPL | ModeHelp

data Options = Options
  { optMode :: Mode
  }

defaultOptions :: Options
defaultOptions = Options
  { optMode = ModeREPL
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h']  ["help"]  (NoArg (\opts -> opts { optMode = ModeHelp }))  "print this help message"
  ]

usage :: String
usage =  "Usage: clp [Options...] filepath\n"

parseOpts :: [String] -> IO (Options, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo usage options

--------------------------------------------------------------------------------
-- | REPL

data Command = Load FilePath | TypeOf String | Quit | Help
  deriving (Show)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseCommand :: String -> Command
parseCommand key
  | ":load" `isPrefixOf` key = (Load . trim . drop 5) key
  | ":l"    `isPrefixOf` key = (Load . trim . drop 2) key
  | ":type" `isPrefixOf` key = (TypeOf . trim . drop 5) key
  | ":t"    `isPrefixOf` key = (TypeOf . trim . drop 2) key
  | otherwise = case trim key of
      ":q"    -> Quit
      ":quit" -> Quit
      _ -> Help

handleCommand :: Command -> IO ()
handleCommand _ = displayHelp
-- handleCommand (Load filePath) = do
-- handleCommand (TypeOf name) = do
-- handleCommand Quit = do
-- handleCommand Help = do

displayHelp :: IO ()
displayHelp = liftIO $ do
  putStrLn "======================================================"
  putStrLn "      _     _          _     _          _     _   "
  putStrLn "     (c).-.(c)        (c).-.(c)        (c).-.(c)  "
  putStrLn "      / ._. \\          / ._. \\          / ._. \\   "
  putStrLn "    __\\( Y )/__      __\\( Y )/__      __\\( Y )/__ "
  putStrLn "   (_.-/'-'\\-._)    (_.-/'-'\\-._)    (_.-/'-'\\-._)"
  putStrLn "      || C ||          || L ||          || P ||   "
  putStrLn "    _.' `-' '._      _.' `-' '._      _.' `-' '._ "
  putStrLn "   (.-./`-'\\.-.)    (.-./`-'\\.-.)    (.-./`-'\\.-.)"
  putStrLn "    `-'     `-'      `-'     `-'      `-'     `-' "
  putStrLn "  :help                 for this help message   (:h)"
  putStrLn "  :load FILEPATH        for loading files       (:l)"
  putStrLn "  :reload               for reloading           (:r)"
  putStrLn "  :type                 for sessions and types  (:t)"
  putStrLn "  :quit                 bye                     (:q)"
  putStrLn "======================================================"
