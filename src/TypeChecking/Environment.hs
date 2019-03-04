module TypeChecking.Environment where

-- import qualified Syntax.Concrete as C
import Syntax.Concrete

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Loc (Loc)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Monad & Types

type Var = TermName Loc

data Index = Pos Int | Neg Int
    deriving (Eq, Show)
-- Props are basically just Types
data Prop = Concrete (Type Loc) | Abstract Index
  deriving (Show)

type Term = Process Loc
type Context = Map Var Prop

data EnvState = EnvState
  { stContext  :: Context
  , stVarCount :: Int
  } deriving (Show)

initialEnvState :: EnvState
initialEnvState = EnvState Map.empty 0

data InferError
  = General String
  | NotInContext Var
  | CannotUnify (Type Loc) (Type Loc)
  deriving (Show)

type EnvM = ExceptT InferError (State EnvState)

putContext :: Context -> EnvM ()
putContext x = modify $ \ st -> st { stContext = x }

--------------------------------------------------------------------------------
-- |

-- contextShouldBeEmpty :: EnvM ()
-- contextShouldBeEmpty = do
--   ctx <- gets stContext
--   unless (Set.null ctx) $ throwError ContextShouldBeEmpty

--                   | Compose   (TermName ann) (Process  ann) (Process ann)   ann
--                   | Output    (TermName ann) (TermName ann) (Process ann) (Process ann) ann
--                   | Input     (TermName ann) (TermName ann) (Process ann)   ann
--                   | SelectL   (TermName ann) (Process  ann)                 ann
--                   | SelectR   (TermName ann) (Process  ann)                 ann
--                   | Choice    (TermName ann) (Process  ann) (Process ann)   ann
--                   | Accept    (TermName ann) (TermName ann) (Process ann)   ann
--                   | Request   (TermName ann) (TermName ann) (Process ann)   ann
--                   | EmptyOutput              (TermName ann)                 ann
--                   | EmptyInput               (TermName ann) (Process ann)   ann
--                   | EmptyChoice

lookup :: Var -> EnvM Prop
lookup var = do
  ctx <- gets stContext
  case Map.lookup var ctx of
    Nothing -> throwError $ NotInContext var
    Just t -> return t

-- substitute type variables with concrete types or other type variables
substitute :: Index -> Prop -> EnvM ()
substitute i t = do
  ctx <- gets stContext
  putContext $ fmap subst ctx
  where
    subst :: Prop -> Prop
    subst (Abstract j) = if i == j then t else Abstract j
    subst others = others

-- refine :: Type -> Type -> EnvM ()
-- refine old new = undefined

unifyOpposite :: Prop -> Prop -> EnvM ()
unifyOpposite (Concrete t) (Concrete u) = unless (t == dual u) $
  throwError $ CannotUnify t u
unifyOpposite (Concrete t) (Abstract u) = substitute u (Concrete (dual t))
unifyOpposite (Abstract t) (Concrete u) = substitute t (Concrete (dual u))
unifyOpposite (Abstract t) (Abstract u) = undefined
    -- substitute t (Abstract (dual u))

infer :: Term -> EnvM ()
infer (Link x y _) = do
  t <- lookup x
  u <- lookup y
  unifyOpposite t u

  -- gets
  -- putContext
