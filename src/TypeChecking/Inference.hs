module TypeChecking.Inference where

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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | State

type Var = C.TermName Loc
type Term = C.Process Loc

-- Γ, Δ, bunch of Var - Type pair
type Context = Map Var Type

data InferState = InferState
  { stContext   :: Context
  -- type variables
  , stTypeSet   :: Set Type         -- bunch of types
  , stTypeCount :: Int              -- for type variables
  -- context variables
  -- , stCtxSet    :: Set Context      -- bunch of contexts
  -- , stCtxCount  :: Int              -- for context variables
  } deriving (Show)

initialInferState :: InferState
initialInferState = InferState Map.empty Set.empty 0  -- Set.empty 0

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  | VarNotAssumed Var
  | VarNotInContext Var Context
  | OverlappedContext Context
  | ContextShouldBeTheSame Context Context
  | CannotUnify Type Type
--   | VarNotFresh Var (Maybe Term)
  deriving (Show)

--------------------------------------------------------------------------------
-- | Monad

type InferM = ExceptT InferError (State InferState)

putContext :: Context -> InferM ()
putContext x = modify $ \ st -> st { stContext = x }

putTypeSet :: Set Type -> InferM ()
putTypeSet x = modify $ \ st -> st { stTypeSet = x }

addToTypeSet :: Type -> InferM ()
addToTypeSet t = modify $ \ st -> st { stTypeSet = Set.insert t (stTypeSet st) }

-- putContexts :: Map Int Context -> InferM ()
-- putContexts x = modify $ \ st -> st { stCtxSet = x }

getFreshType :: InferM Type
getFreshType = do
  i <- gets stTypeCount
  modify $ \ st -> st { stTypeCount = i + 1 }
  return $ Var (Pos i)

-- addToContext :: Var -> Prop -> InferM ()
-- addToContext var prop = modify $ \ st -> st { stContext = Map.insert var prop (stContext st) }

--------------------------------------------------------------------------------
-- |

infer :: Term -> InferM Context
infer (C.Link x y _) = do
  t <- assumeType
  return $ Map.fromList
    [ (x,      t)
    , (y, dual t)
    ]
infer (C.Compose x p q _) = do
  (a, ctxP) <- infer p >>= splitCtx x
  (b, ctxQ) <- infer q >>= splitCtx x
  checkOverlappedContext ctxP ctxQ
  unifyOpposite a b
  return $ mergeContext ctxP ctxQ
infer (C.Output x y p q _) = do
  (a, ctxP) <- infer p >>= splitCtx y
  (b, ctxQ) <- infer q >>= splitCtx x
  return
    $ Map.insert x (Times a b)
    $ mergeContext ctxP ctxQ

infer (C.Input x y p _) = do
  (b, ctx) <- infer p >>= splitCtx x
  (a, ctx') <- splitCtx y ctx
  return $ Map.insert x (Par a b)
    $ ctx'

infer (C.SelectL x p _) = do
  (a, ctx) <- infer p >>= splitCtx x
  b <- assumeType
  return $ Map.insert x (Plus a b) ctx

infer (C.SelectR x p _) = do
  (b, ctx) <- infer p >>= splitCtx x
  a <- assumeType
  return $ Map.insert x (Plus a b) ctx

infer (C.Choice x p q _) = do
  (a, ctxP) <- infer p >>= splitCtx x
  (b, ctxQ) <- infer q >>= splitCtx x
  contextShouldBeTheSame ctxP ctxQ
  return $ Map.insert x (With a b) ctxP

infer (C.Accept x y p _) = undefined
infer (C.Request x y p _) = undefined
infer (C.EmptyOutput x _) = do
  return $ Map.fromList
    [ (x, One)
    ]
infer (C.EmptyInput x p _) = do
  a <- infer p
  return $ Map.insert x Bot a
infer (C.EmptyChoice x _) = undefined
  -- return $ Map.insert x Top
  -- a <- infer p
  -- b <- infer q


splitCtx :: Var -> Context -> InferM (Type, Context)
splitCtx var ctx = do
  case Map.lookup var ctx of
    Nothing -> throwError $ VarNotInContext var ctx
    Just t -> return (t, Map.delete var ctx)


assumeType :: InferM Type
assumeType = do
  t <- getFreshType
  addToTypeSet t
  return t


-- unsafe, does not check if contexts are overlapping
mergeContext :: Context -> Context -> Context
mergeContext = Map.merge
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.zipWithMaybeMatched (\_ x _ -> Just x))

-- lookupType :: Var -> InferM Type
-- lookupType var = do
--   types <- gets stTypes
--   case Map.lookup var types of
--     Nothing -> throwError $ VarNotAssumed var
--     Just t -> return t

-- substitute type variables with concrete types or other type variables
substituteType :: Index -> Type -> InferM ()
substituteType i t = do
  context <- gets stContext
  putContext $ fmap substType context
  types <- gets stTypeSet
  putTypeSet $ Set.fromList $ map substType $ Set.toList types
  -- ctxs <- gets stCtxSet
  -- putContexts $ fmap substCtx  ctxs

  where
    substType :: Type -> Type
    substType (Var j) = if i == j then t else Var j
    substType others = others

    -- substCtx :: Context -> Context
    -- substCtx ctx = fmap substType ctx

unifyOpposite :: Type -> Type -> InferM (Type, Type)
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

checkOverlappedContext :: Context -> Context -> InferM ()
checkOverlappedContext a b = do
  let overlapped = Map.intersection a b
  unless (Map.null overlapped) $
    throwError $ OverlappedContext overlapped

contextShouldBeTheSame :: Context -> Context -> InferM ()
contextShouldBeTheSame a b = do
  let overlapped = Map.difference a b
  unless (Map.null overlapped) $
    throwError $ ContextShouldBeTheSame a b

-- contextShouldBeEmpty :: InferM ()
-- contextShouldBeEmpty = do
--   ctx <- gets stContext
--   unless (Set.null ctx) $ throwError ContextShouldBeEmpty
