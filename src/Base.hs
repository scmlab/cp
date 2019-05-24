{-# LANGUAGE OverloadedStrings                  #-}
module Base where

import qualified Syntax.Abstract as A
import qualified Syntax.Concrete as C
import Syntax.Parser
import TypeChecking.Types

import Data.ByteString.Lazy (ByteString)
import Data.Loc (Loc(..))

import Control.Monad.State
import Control.Monad.Except

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

runM :: M a -> IO (Either Error a, MState)
runM program = runStateT (runExceptT program) initialState
    where
        initialState = MState Nothing Nothing

evalM :: M a -> IO (Either Error a)
evalM program = evalStateT (runExceptT program) initialState
    where
        initialState = MState Nothing Nothing
