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
import Data.List (dropWhileEnd, isPrefixOf, find)

-- import qualified Data.ByteString.Lazy.Char8 as BC

import Control.Exception (IOException, try)
import Control.Monad.State
import Control.Monad.Except

import System.Console.Haskeline
import System.Console.GetOpt
import System.Environment
import Debug.Trace

-- putSource :: Maybe ByteString -> M ()
-- putSource x = modify $ \ st -> st { stSource = x }

getSourceOrThrow :: M (String, ByteString)
getSourceOrThrow = do
  result <- gets stSource
  case result of
    Nothing -> throwError $ Panic "Can't read the stored source code"
    Just v -> return v

--------------------------------------------------------------------------------
-- |

loadSource :: FilePath -> M ()
loadSource filePath = do
  readResult <- liftIO $ try (BS.readFile filePath)
  case readResult of
      Left err -> throwError $ Panic $ show (err :: IOException)
      Right source -> do
        -- store the source in the monad for later debugging use
        modify $ \ st -> st { stSource = Just (filePath, source) }

parseSource :: M ()
parseSource = do
  (filePath, source) <- getSourceOrThrow
  case parseConcreteProgram filePath source of
      Left err -> throwError $ ParseError err
      Right cst -> do
        modify $ \ st -> st { stConcrete = Just cst }

handleM :: M a -> IO (Maybe a)
handleM program = do
  (result, state) <- runM program
  case result of
    Left err -> do
      case stSource state of
        Nothing -> print err
        Just (_, source) -> putDoc $ prettyError err source
      return Nothing
    Right value -> return (Just value)



runTCM :: TCM a -> M (a, TCState)
runTCM program = do
    let (result, s) = runState (runExceptT program) initialTCState
    case result of
      Left err -> throwError $ TypeError err
      Right val -> return (val, s)

commands :: [String]
commands = [ ":load", ":reload", ":type", ":quit", ":help" ]

data Matching = Complete String | Partial [String] | Over String [String] | None

match :: String -> Matching
match raw = case words (reverse raw) of
  [] -> None
  (x:xs) -> if elem x commands
              then if null xs then Complete x else Over x xs
              else case filter (isPrefixOf x) commands of
                [] -> None
                matched -> Partial matched


main :: IO ()
main = do
  (opts, _filePaths) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> void $ handleM $ runInputT settings loop
  where

    settings :: Settings M
    settings = setComplete complete defaultSettings

    complete :: CompletionFunc M
    complete (left, right) = case match left of
      Complete ":load" -> completeFilename (left, right)
      Complete ":reload" -> completeFilename (left, right)
      Complete _ -> return (left, [Completion "" "" False])
      Partial matched -> return ("", map simpleCompletion matched)
      Over ":load" _ -> completeFilename (left, right)
      Over ":reload" _ -> completeFilename (left, right)
      Over _ _ -> return (left, [Completion "" "" False])
      None -> completeCommands

    completeCommands :: M (String, [Completion])
    completeCommands = return ("", map simpleCompletion commands)

    -- complete (left, right) = completeFilename (left, right)
    -- complete (" daol:", right) = completeFilename (" daol:", right)
    --
    --
    -- complete (left, right) = case words (reverse left) of
    --   [] -> completeFilename (left, right)
    --   (x:_) -> if isCompleted x
    --     then completeFilename (reverse (" " ++ x), right)
    --     else case matched x of
    --       [] -> completeFilename (left, right)
    --       result -> return ("", map simpleCompletion result)
    --
    --   where
    --     commands' = map ((:) ':') commands
    --     isCompleted x = elem x commands'
    --     matched x = filter (isPrefixOf x) commands'



    loop :: InputT M ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> do
          -- outputStrLn $ show $ parseCommand input
          keepLooping <- liftIO $ handleCommand $ parseCommand input
          when keepLooping loop
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

data Command = Load FilePath | TypeOf String | Quit | Help | Noop
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
      _ -> Noop

handleCommand :: Command -> IO Bool
handleCommand (Load filePath) = do
  result <- handleM $ do
    loadSource filePath
    parseSource
    return ()
  case result of
    Nothing -> return ()
    Just value -> putStrLn $ "loaded: " ++ filePath

  return True
handleCommand (TypeOf name) = return True
handleCommand Quit = return False
handleCommand Help = displayHelp >> return True
handleCommand Noop = return True

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
