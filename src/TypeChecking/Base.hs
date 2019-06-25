module TypeChecking.Base where

import qualified Syntax.Concrete as C
import Syntax.Binding
--
import Prelude hiding (lookup)


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------------------------
-- | State

isAnnotated :: Definition -> Bool
isAnnotated (Annotated _ _ _) = True
isAnnotated _                 = False

type CtxVar = Int

data TCState = TCState
  { stTypeCount   :: Int              -- for type variables
  , stDefinitions :: Map Name Definition
  } deriving (Show)

initialTCState :: TCState
initialTCState = TCState 0 Map.empty


--------------------------------------------------------------------------------
-- | Error

data TypeError
  = General Text
  | TypeMismatch Process Type Type Type Type
  | SessionMismatch Name Session Session
  | SessionShouldBeTheSame Process Session
  | SessionShouldBeDisjoint Process Session
  | SessionShouldAllBeRequesting Process Session
  | CannotCloseChannel Process Chan
  | ChannelNotFound Process Chan Session
  | ChannelNotComsumed Process Session
  | DefnNotFound Process Name
  deriving (Show)

data ScopeError
  = TypeSigDuplicated C.Name C.Name
  | TermDefnDuplicated C.Name C.Name
  | Others Text
  deriving (Show)

--------------------------------------------------------------------------------
-- | TCM

type TCM = ExceptT TypeError (State TCState)
