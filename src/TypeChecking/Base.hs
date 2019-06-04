module TypeChecking.Base where

-- import Syntax.Concrete
import qualified Syntax.Concrete as C
import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
import Syntax.Abstract (Session, Type(..))
--
import Prelude hiding (lookup)

import Data.Loc (Loc)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------------------------
-- | State

type Name = TermName Loc
type Chan = Name
type Term = Process Loc

data Definition = Annotated   Name Term (C.Session Loc)
                | Unannotated Name Term
                deriving (Show)

isAnnotated :: Definition -> Bool
isAnnotated (Annotated _ _ _) = True
isAnnotated _                 = False

type CtxVar = Int

data TCState = TCState
  { stTypeCount   :: Int              -- for type variables
  , stDefinitions :: Map Text Definition
  } deriving (Show)

initialTCState :: TCState
initialTCState = TCState 0 Map.empty


--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  | TypeMismatch Term Type Type Type Type
  | SessionMismatch Name Session Session
  | CannotAppearInside Term Chan
  | SessionShouldBeTheSame Term Session
  | SessionShouldBeDisjoint Term Session
  | SessionShouldAllBeRequesting Term Session
  | ChannelNotFound Term Chan Session
  | ChannelNotComsumed Term Session
  | DefnNotFound Term Name
  deriving (Show)

data TypeError
  = TypeSigDuplicated Name Name
  | TermDefnDuplicated Name Name
  | InferError InferError
  | Others Text
  deriving (Show)

--------------------------------------------------------------------------------
-- | TCM

type TCM = ExceptT TypeError (State TCState)
