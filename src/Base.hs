{-# LANGUAGE OverloadedStrings                  #-}
module Base where

import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Parser
import TypeChecking.Types

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Loc (Loc(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import System.Console.Haskeline
import System.Console.Haskeline.MonadException

--------------------------------------------------------------------------------
-- | The M Monad

data MState = MState
  { replSource    :: Maybe (String, ByteString)
  , replConcrete  :: Maybe (C.Program Loc)
  , replInferred  :: Map Text A.Session
  } deriving (Show)

data Error = ParseError ParseError
           | TypeError TypeError
           | Panic String
           deriving (Show)

type M = ExceptT Error (StateT MState IO)


initialState :: MState
initialState = MState Nothing Nothing Map.empty

runM :: M a -> IO (Either Error a, MState)
runM program = runStateT (runExceptT program) initialState

evalM :: M a -> IO (Either Error a)
evalM program = evalStateT (runExceptT program) initialState

--------------------------------------------------------------------------------
-- | The REPL Monad

type Core = StateT MState IO
type REPL = InputT Core

runREPL :: Settings Core -> REPL a -> IO (a, MState)
runREPL settings program = runStateT (runInputT settings program) initialState

-- instances of Haskeline.MonadException
instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'


instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'