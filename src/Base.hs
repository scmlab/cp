{-# LANGUAGE OverloadedStrings                  #-}
module Base where

import qualified Syntax.Abstract as A
-- import qualified Syntax.Binding as B
-- import qualified Syntax.Concrete as C
-- import Syntax.Binding
import Syntax.Concrete
import Syntax.Parser
import TypeChecking.Base

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import System.Console.Haskeline

--------------------------------------------------------------------------------
-- | The M Monad

data MState = MState
    -- filepath and the source  (for parsing)
  { replSource        :: Maybe (String, ByteString)
    -- parsed program           (for scope checking)
  , replProgram       :: Maybe Program
    -- scoped checked program   (for type checking)
  , replDefinitions   :: Map Name Definition
    -- inferred program         (for runtime execution)
  , replInferred      :: Map Name Session
  } deriving (Show)

data Error = ParseError ParseError
           | RuntimeError RuntimeError
           | TypeError TypeError
           | ScopeError ScopeError
           | Panic String
           deriving (Show)

type M = ExceptT Error (StateT MState IO)


initialState :: MState
initialState = MState Nothing Nothing Map.empty Map.empty

runM :: M a -> IO (Either Error a, MState)
runM program = runStateT (runExceptT program) initialState

evalM :: M a -> IO (Either Error a)
evalM program = evalStateT (runExceptT program) initialState

--------------------------------------------------------------------------------
-- | The REPL Monad

type REPL = InputT M

runREPL :: Settings M -> REPL a -> IO (Either Error a, MState)
runREPL settings program = runStateT (runExceptT (runInputT settings program)) initialState

-- instances of Haskeline.MonadException
instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'


instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'

--------------------------------------------------------------------------------
-- | Runtime Error

data RuntimeError
  = Runtime_NotInScope Name
  | Runtime_CodeNotLoaded
  | Runtime_CannotMatch A.Process [A.Chan]
  | Runtime_Stuck A.Process

  deriving (Show)
-- data Rule = AxCut Chan


abstract :: A.FromConcrete a b => Maybe Definitions -> a -> M b
abstract definitions process = do
  let result = A.runAbstractM (maybe Map.empty id definitions) (A.fromConcrete process)
  case result of
    Left err -> throwError $ ScopeError err
    Right val -> return val
