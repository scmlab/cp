{-# LANGUAGE OverloadedStrings                  #-}
module Base where

import qualified Syntax.Concrete as C
import Syntax.Parser
import TypeChecking.Types

import Data.ByteString.Lazy (ByteString)
import Data.Loc (Loc(..))

import Control.Monad.State
import Control.Monad.Except
import System.Console.Haskeline.MonadException

--------------------------------------------------------------------------------
-- | The M Monad

data MState = MState
  { stSource    :: Maybe (String, ByteString)
  , stConcrete  :: Maybe (C.Program Loc)
  } deriving (Show)

data Error = ParseError ParseError
           | TypeError TypeError
           | Panic String
           deriving (Show)

type M = ExceptT Error (StateT MState IO)

-- instances of Haskeline.MonadException
instance MonadException m => MonadException (StateT s m) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'


instance (MonadException m) => MonadException (ExceptT e m) where
    controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap ExceptT . run . runExceptT)
                    in fmap runExceptT $ f run'

runM :: M a -> IO (Either Error a, MState)
runM program = runStateT (runExceptT program) initialState
    where
        initialState = MState Nothing Nothing

evalM :: M a -> IO (Either Error a)
evalM program = evalStateT (runExceptT program) initialState
    where
        initialState = MState Nothing Nothing
