{-# LANGUAGE OverloadedStrings                  #-}
module Main where

import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Parser
import TypeChecking
import TypeChecking.Inference
import Pretty

import Data.Monoid (mempty, (<>))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Loc (Loc(..), locOf)
import qualified Data.Loc as Loc
import Data.Text (Text, pack)
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

    (tc, _) <- runTCM (checkAll program)
    liftIO $ print tc
    return ()

--------------------------------------------------------------------------------
-- | Printing errors

prettyError' :: Text -> [Doc AnsiStyle] -> [Loc] -> M (Doc AnsiStyle)
prettyError' header paragraphs locations = do
  let text = vsep $
        [ annotate (color Red) $ pretty header
        , softline
        ]
        ++ (map (\ x -> x <> line) paragraphs)
  source <- getSourceOrThrow
  let pieces = vsep $ map (\loc -> vsep
        [ annotate (colorDull Blue) (pretty $ Loc.displayLoc loc)
        , reAnnotate toAnsiStyle $ prettySourceCode $ SourceCode source loc 1
        ]) locations

  return $ vsep
    [ softline'
    , indent 2 text
    , indent 2 pieces
    , softline'
    ]
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
    [locOf pos]
prettyParseError (Syntatical loc _) = do
  prettyError "Lexical parse error" Nothing
  -- (Just $ pack $ "got " ++ show token)
    [loc]

prettyTypeError :: TypeError -> M (Doc AnsiStyle)
prettyTypeError (TypeSigDuplicated a b) =
  prettyError "Duplicating type signature" Nothing
    [locOf a, locOf b]
prettyTypeError (TermDefnDuplicated a b) =
  prettyError "Duplicating term definition" Nothing
    [locOf a, locOf b]
prettyTypeError (TypeSigNotFound a) =
  prettyError "Missing type signature" (Just "the following term has no type signature")
    [locOf a]
prettyTypeError (TermDefnNotFound a) =
  prettyError "Missing term definition" (Just "the following type has no term definition")
    [locOf a]
prettyTypeError (InferError a) =
   prettyInferError a
prettyTypeError (Others msg) =
  prettyError "Other unformatted type errors" (Just msg) []

prettyInferError :: InferError -> M (Doc AnsiStyle)
prettyInferError (General msg) = prettyError "Other unformatted inference errors" (Just msg) []
-- prettyInferError (ChannelsNotInContext term chan context) = prettyError "Channel not in context" (Just $ pack $ show chan ++ "\n" ++ show context) [locOf term]
prettyInferError (ChannelsNotInContext term chan context) =
  prettyError' "Channel not in context"
    [ "these channels "
        <> (annotate (colorDull Blue) (pretty chan))
        <> " are not in the context"
        <> line
        <> pretty context
        <> line
        <> "when checking the following term"
    ] [locOf term]
prettyInferError (ShouldBeTypeVar v) = prettyError "Channel type should be some variable" (Just $ pack $ show v) []
prettyInferError (CannotUnify a b) = prettyError "Cannot unify" (Just $ pack $ show a ++ "\n" ++ show b) []
