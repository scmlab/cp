{-# LANGUAGE OverloadedStrings                  #-}
module Main where

-- import Syntax.Base
-- import qualified Syntax.Concrete as C
import Syntax.Concrete
import Syntax.Parser
import TypeChecking
-- import TypeChecking.Binding
-- import TypeChecking.Infer
import TypeChecking.Infer (inferProcess)
import TypeChecking.Base
import Pretty.Error ()
import Pretty.Base
import Base
import Runtime

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Text.Prettyprint.Doc.Render.Terminal

import qualified Data.Map as Map
-- import qualified Data.Set as Set
import Data.Maybe (isNothing)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc

-- import qualified Data.ByteString.Lazy.Char8 as BC

import Control.Exception (IOException, try)
import Control.Monad.State hiding (state)
import Control.Monad.Except
import Control.Monad.Reader

import System.Console.Haskeline
import System.Console.GetOpt
import System.Environment
-- import Debug.Trace


-- putSource :: Maybe ByteString -> M ()
-- putSource x = modify $ \ st -> st { stSource = x }

getSourceOrThrow :: M (String, ByteString)
getSourceOrThrow = do
  result <- gets replSource
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
        modify $ \ st -> st { replSource = Just (filePath, source) }

parseSource :: M Program
parseSource = do
  (filePath, source) <- getSourceOrThrow
  case parseProgram filePath source of
      Left err -> throwError $ ParseError err
      Right program -> do
        modify $ \ st -> st { replProgram = Just program }
        return program

parseProcessM :: ByteString -> M Process
parseProcessM raw = do
  case parseProcess raw of
    Left err -> throwError $ ParseError err
    Right process -> return process

printErrorIO :: MState -> Error -> IO ()
printErrorIO state err = case replSource state of
  Nothing -> putDoc $ report err
  Just (_, source) -> putDoc $ reportS err (Just source)

printError :: Error -> REPL ()
printError err = do
  state <- lift get
  liftIO $ printErrorIO state err

handleM :: M a -> REPL (Maybe a)
handleM program = do
  result <- lift $ runExceptT program
  case result of
    Left err -> do
      printError err
      return Nothing
    Right value -> return (Just value)

runTCM :: TCM a -> M a
runTCM f = do
  definitions <- gets replDefinitions
  let result = runReader (runExceptT f) definitions
  case result of
    Left err -> throwError $ TypeError err
    Right val -> return val


main :: IO ()
main = do
  (opts, _filePaths) <- getArgs >>= parseOpts
  case optMode opts of
    ModeHelp -> putStrLn $ usageInfo usage options
    ModeREPL -> void $ runREPL settings loop
    ModeDev -> void $ runREPL settings $ do
      _ <- handleCommand $ parseCommand ":l test/source/a.clp"
      loop
      -- void $ runInputT settings loop
  where

    settings :: Settings Core
    settings = setComplete customComplete defaultSettings

    customComplete :: CompletionFunc Core
    customComplete (left, right) = case match left of
      Complete ":load" -> completeFilename (left, right)
      Complete ":reload" -> completeFilename (left, right)
      Complete ":type" -> completeDefinitions left []
      Complete _ -> return (left, [Completion "" "" False])
      Partial matched -> return ("", map simpleCompletion matched)
      Over ":load" _ -> completeFilename (left, right)
      Over ":reload" _ -> completeFilename (left, right)
      Over ":type" xs -> completeDefinitions left xs
      Over _ _ -> return (left, [Completion "" "" False])
      None "" -> completeCommands
      None _ -> completeDefinitions left []

    completeDefinitions :: String -> [String] -> Core (String, [Completion])
    completeDefinitions left partials = do
      -- get the names of all definitions
      defns <- map (Text.unpack . (\(Name name _) -> name)) <$> Map.keys <$> gets replDefinitions
      -- complete only the last chuck
      let partial = if null partials then "" else last partials
      let matched = case filter (isPrefixOf partial) defns of
                      [] -> defns
                      xs -> xs
      let left' = if null partials then left else dropWhile (/= ' ') left
      return (left', map simpleCompletion matched)

    completeCommands :: Core (String, [Completion])
    completeCommands = return ("", map simpleCompletion commands)

    loop :: REPL ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> do
          keepLooping <- handleCommand $ parseCommand input
          when keepLooping loop


--------------------------------------------------------------------------------
-- | Command-line arguments

data Mode = ModeREPL | ModeHelp | ModeDev

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
  , Option ['d']  ["dev"]  (NoArg (\opts -> opts { optMode = ModeDev }))   "for testing"
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

data Command = Load FilePath | Reload | TypeOf ByteString | Eval ByteString | Debug ByteString | Quit | Help | Noop
  deriving (Show)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseCommand :: String -> Command
parseCommand key
  | ":load" `isPrefixOf` key = (Load . trim . drop 5) key
  | ":l"    `isPrefixOf` key = (Load . trim . drop 2) key
  | ":reload" `isPrefixOf` key = Reload
  | ":r"    `isPrefixOf` key = Reload
  | ":type" `isPrefixOf` key = (TypeOf . BS8.pack . trim . drop 5) key
  | ":t"    `isPrefixOf` key = (TypeOf . BS8.pack . trim . drop 2) key
  | ":debug" `isPrefixOf` key = (Debug . BS8.pack . trim . drop 5) key
  | ":d"    `isPrefixOf` key = (Debug . BS8.pack . trim . drop 2) key
  | otherwise = case trim key of
      ":q"    -> Quit
      ":quit" -> Quit
      ""      -> Noop
      s       -> Eval (BS8.pack s)
        -- traceShow "!!!" (Noop)

whenLoaded :: REPL () -> REPL ()
whenLoaded program = do
  result <- lift $ gets replSource
  if (isNothing result)
    then void $ handleM $ throwError $ RuntimeError $ Runtime_CodeNotLoaded
    else program


handleCommand :: Command -> REPL Bool
handleCommand (Load filePath) = do
  void $ handleM $ do
    loadSource filePath
    program <- parseSource
    definitions <- scopeCheck program
    inferred <- runTCM $ typeCheck definitions
    modify $ \ st -> st
      { replInferred = inferred
      , replDefinitions = definitions
      }
    return ()
    -- liftIO $ putStrLn $ "loaded: " ++ filePath
  return True
handleCommand Reload = do
  result <- handleM $ gets replSource
  case result of
    Just (Just (filePath, _)) -> handleCommand (Load filePath)
    _ -> handleCommand Noop

handleCommand (TypeOf expr) = do
  void $ handleM $ do
    -- global environment setup
    program <- gets replProgram
    -- local expression parsing
    process <- parseProcessM expr
    -- infer session
    session <- runTCM $ inferProcess process
    liftIO $ putDoc $ report session <> line
    return ()
  return True

handleCommand (Debug expr) = do
  void $ handleM $ do
    -- global environment setup
    program <- gets replProgram
    -- local expression parsing
    result <- parseProcessM expr
    -- print some stuff
    liftIO $ putDoc $ pretty result <> line
    return ()
  return True

handleCommand Quit = return False
handleCommand Help = liftIO displayHelp >> return True
handleCommand (Eval expr) = do
  void $ handleM $ do
    -- global environment setup
    program <- gets replProgram
    -- local expression parsing
    process <- parseProcessM expr
    --
    _result <- evaluate process
    -- liftIO $ putDoc $ pretty result <> line
    return ()
  return True
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

commands :: [String]
commands = [ ":load", ":reload", ":type", ":quit", ":help" ]

data Matching = Complete String | Partial [String] | Over String [String] | None String
  deriving (Show)

match :: String -> Matching
match raw = case words (reverse raw) of
  [] -> None ""
  (x:xs) -> if elem x commands
              then if null xs then Complete x else Over x xs
              else case filter (isPrefixOf x) commands of
                [] -> None raw
                matched -> if length x == 2
                  then if null xs then Complete (head matched) else Over (head matched) xs
                  else Partial matched
