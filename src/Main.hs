{-# LANGUAGE OverloadedStrings                  #-}
module Main where

import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Parser
import TypeChecker
import Pretty

import Data.Monoid (mempty, (<>))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Loc (Loc(..))
import qualified Data.Loc as Loc
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

-- import qualified Data.ByteString.Lazy.Char8 as BC

import Control.Exception (IOException, try)
import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------------------------
-- | The M Monad

data MState = MState
  { stSource   :: Maybe ByteString
  , stAbstract :: Maybe A.Program
  , stConcrete :: Maybe (C.Program Loc)
  } deriving (Show)

data Error = ParseError ParseError
           | TypeError TypeError
           | Panic String
           deriving (Show)

type M = ExceptT Error (StateT MState IO)

runM :: M a -> IO (Either Error a, MState)
runM program = runStateT (runExceptT program) initialState
    where
        initialState = MState Nothing Nothing Nothing

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
handleError program = program `catchError` \ err -> case err of
  ParseError parseError -> prettyParseError parseError >>= liftIO . putDoc
  TypeError typeError -> prettyTypeError typeError >>= liftIO . putDoc
  Panic msg -> liftIO (putStrLn (show msg))


runTCM :: TCM a -> M (a, TCState)
runTCM program = do
    let (result, s) = runState (runExceptT program) initialTCState
    case result of
      Left err -> throwError $ TypeError err
      Right val -> return (val, s)

main :: IO ()
main = void $ runM $ handleError $ do
    let filePath = "test/source/a.clp"
    program <- readSource filePath >>= parseSource filePath

    tc <- runTCM $ do
      checkDuplications program
    liftIO $ print tc
    liftIO $ print program
    return ()

--------------------------------------------------------------------------------
-- | Printing errors

prettyError :: Text -> Maybe Text -> [Loc] -> M (Doc AnsiStyle)
prettyError header message locations = do
  let text = vsep
        [ annotate (color Red) $ pretty header
        , case message of
            Just m -> pretty m <> line
            Nothing -> mempty
        ]
  source <- getSourceOrThrow
  let pieces = vsep $ map (\loc -> vsep
        [ annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
        , reAnnotate toAnsiStyle $ prettySourceCode $ SourceCode source loc 1
        ]) locations

  return $ vsep
    [ softline'
    , indent 2 text
    , indent 4 pieces
    , softline'
    ]

prettyParseError :: ParseError -> M (Doc AnsiStyle)
prettyParseError (Lexical pos) = do
  prettyError "Lexical parse error" Nothing
    [Loc.locOf pos]
prettyParseError (Syntatical loc _) = do
  prettyError "Lexical parse error" Nothing
  -- (Just $ pack $ "got " ++ show token)
    [loc]

prettyTypeError :: TypeError -> M (Doc AnsiStyle)
prettyTypeError (TypeSigDuplicated a b) =
  prettyError "Type signature duplicated" Nothing
    [Loc.locOf a, Loc.locOf b]
prettyTypeError (TermDefnDuplicated a b) =
  prettyError "Term definition duplicated" Nothing
    [Loc.locOf a, Loc.locOf b]
prettyTypeError (Others msg) =
  prettyError "Other unformatted type errors" (Just msg) []
