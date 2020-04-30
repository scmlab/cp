module TypeChecking.Base where

-- import qualified Syntax.Concrete as C
-- import Syntax.Binding
import           Syntax.Concrete
--
import           Prelude                 hiding ( lookup )


import           Data.Text                      ( Text )
import           Data.Loc                       ( Loc )

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Lazy

--------------------------------------------------------------------------------
-- | State

isAnnotated :: Definition -> Bool
isAnnotated (Paired _ _ _) = True
isAnnotated _              = False

-- type CtxVar = Int
--
-- data TCState = TCState
--   { stTypeCount   :: Int              -- for type variables
--   , stDefinitions :: Map Name Definition
--   } deriving (Show)

-- initialTCState :: TCState
-- initialTCState = TCState 0 Map.empty


--------------------------------------------------------------------------------
-- | Error

data TypeError
  =  TypeMismatch Process Type Type Type Type
  -- | SessionMismatch Name Session Session
  -- | SessionShouldBeTheSame Process Session
  -- | SessionShouldBeDisjoint Process Session
  -- | SessionShouldAllBeRequesting Process Session
  -- | CannotCloseChannel Process Chan
  -- | ChannelNotFound Process Chan Session
  -- | ChannelNotComsumed Process Session
  -- | ChanFound Process Chan
  | ChannelNotComsumed Process Chan


  | SessionMismatch Process Session Session
  | SessionNotDisjoint Process Session Session
  | SessionNotAllRequest Process Session
  deriving (Show)

data ScopeError
  = TypeSigDuplicated Name Name
  | TermDefnDuplicated Name Name
  | DefnNotFound Name
  | RecursiveCall [Name]
  -- | ChanNotFound Process (Set Text)
  -- | ChanFound Process (Set Text)
  | Others Text
  deriving (Show)

--------------------------------------------------------------------------------
-- | TCM

type TCM = ExceptT TypeError (StateT Int (Reader Definitions))

--
-- --------------------------------------------------------------------------------
-- -- | Type Checking Monad
--
-- type TypeM = ExceptT TypeError (State TCState)
