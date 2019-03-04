module TypeChecking.Environment where

import qualified Syntax.Concrete as C
import Syntax.Concrete (HasDual(..))
import Syntax.TypeChecking

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Loc (Loc)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Monad & Types

type Var = C.TermName Loc
type Term = C.Process Loc

-- Γ, Δ, bunch of Var - Type pair
type Context = Map Var Type

data EnvState = EnvState
  { stContexts  :: Map Int Context  -- bunch of contexts
  , stCtxCount  :: Int              -- for context variables
  , stTypes     :: Context          -- bunch of types
  , stTypeCount :: Int              -- for type variables
  } deriving (Show)

-- initialEnvState :: EnvState
-- initialEnvState = EnvState Map.empty 0
--
data InferError
  = General String
  | VarNotFoundInTypes Var
  | OverlappedContext Context
  | CannotUnify Type Type
--   | VarNotFresh Var (Maybe Term)
  deriving (Show)
--
type EnvM = ExceptT InferError (State EnvState)

putTypes :: Context -> EnvM ()
putTypes x = modify $ \ st -> st { stTypes = x }

putContexts :: Map Int Context -> EnvM ()
putContexts x = modify $ \ st -> st { stContexts = x }

-- getFreshTypeVar :: EnvM Prop
-- getFreshTypeVar = do
--   i <- gets stVarCount
--   modify $ \ st -> st { stVarCount = i + 1 }
--   return $ Abstract (Pos i)
--
-- addToContext :: Var -> Prop -> EnvM ()
-- addToContext var prop = modify $ \ st -> st { stContext = Map.insert var prop (stContext st) }

--------------------------------------------------------------------------------
-- |

-- contextShouldBeEmpty :: EnvM ()
-- contextShouldBeEmpty = do
--   ctx <- gets stContext
--   unless (Set.null ctx) $ throwError ContextShouldBeEmpty

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


infer :: Term -> EnvM Context
infer (C.Link x y _) = do
  t <- lookupType x
  u <- lookupType y
  (t', u') <- unifyOpposite t u
  return $ Map.fromList
    [ (x, t')
    , (y, u')
    ]
infer (C.Compose x p q _) = do
  a <- Map.delete x <$> infer p
  b <- Map.delete x <$> infer q
  checkOverlappedContext a b
  return $ mergeContext a b
-- infer (C.Output x y p q _) = do
--   a <- infer p
--   b <- infer q


-- unsafe, does not check if contexts are overlapping
mergeContext :: Context -> Context -> Context
mergeContext = Map.merge
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.zipWithMaybeMatched (\_ x _ -> Just x))

lookupType :: Var -> EnvM Type
lookupType var = do
  types <- gets stTypes
  case Map.lookup var types of
    Nothing -> throwError $ VarNotFoundInTypes var
    Just t -> return t

-- substitute type variables with concrete types or other type variables
substituteType :: Index -> Type -> EnvM ()
substituteType i t = do
  types <- gets stTypes
  putTypes    $ fmap substType types
  ctxs <- gets stContexts
  putContexts $ fmap substCtx  ctxs

  where
    substType :: Type -> Type
    substType (Var j) = if i == j then t else Var j
    substType others = others

    substCtx :: Context -> Context
    substCtx ctx = fmap substType ctx

unifyOpposite :: Type -> Type -> EnvM (Type, Type)
unifyOpposite (Var i) (Var j) = do
  let i' = Var (dual i)
  -- replace j with the opposite of i
  substituteType j i'
  return (Var i, i')
unifyOpposite (Var i) u = do
  substituteType i (dual u)
  return (dual u, u)
unifyOpposite t (Var i) = do
  substituteType i (dual t)
  return (t, dual t)
unifyOpposite t u = if t == dual u
  then throwError $ CannotUnify t u
  else return (t, u)

checkOverlappedContext :: Context -> Context -> EnvM ()
checkOverlappedContext a b = do
  let overlapped = Map.difference a b
  unless (Map.null overlapped) $
    throwError $ OverlappedContext overlapped
