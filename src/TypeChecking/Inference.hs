module TypeChecking.Inference where

import qualified Syntax.Concrete as C
import Syntax.Abstract (Type(..))
import Syntax.Base

import Prelude hiding (lookup)
-- import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.List as List
import Data.Loc (Loc, locOf)
-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | State

type Chan = C.TermName Loc
type Term = C.Process Loc

type CtxVar = Int

-- pairs of channels and their type
type Context = Map Chan Type
data CrudeSession = Crude Context CtxVar
data Session = Session Context (Set CtxVar)
  deriving (Show)

data InferState = InferState
  {
    stTarget    :: Session
  , stTypeCount :: Int              -- for type variables
  , stCtxCount  :: Int              -- for context variables
  } deriving (Show)

initTarget :: Session
initTarget = Session Map.empty (Set.singleton 0)

initInferState :: InferState
initInferState = InferState initTarget 0 1  -- Set.empty 0

freshType :: InferM TypeVar
freshType = do
  i <- gets stTypeCount
  modify $ \ st -> st { stTypeCount = i + 1 }
  return $ Pos i

freshCtx :: InferM CtxVar
freshCtx = do
  i <- gets stCtxCount
  modify $ \ st -> st { stCtxCount = i + 1 }
  return $ i

modifyPairs :: (Context -> Context) -> InferM ()
modifyPairs f = do
  Session pairs contexts <- gets stTarget
  modify $ \ st -> st { stTarget = Session (f pairs) contexts }

modifyContexts :: (Set CtxVar -> Set CtxVar) -> InferM ()
modifyContexts f = do
  Session pairs contexts <- gets stTarget
  modify $ \ st -> st { stTarget = Session pairs (f contexts) }

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  -- | ToManyContextVariables Term (Set CtxVar)
  -- | VarNotAssumed Var
  | ChannelsNotInContext Term (Set Chan) Context
  -- | ChannelNotUsed Context
  | ShouldBeTypeVar Type
  -- | OverlappedContext Session
  -- | ContextShouldBeTheSame Session Session
  -- | ContextShouldAllBeRequests Context
  | CannotUnify Type Type
  -- | CannotUnifyContext Context Context
--   | VarNotFresh Var (Maybe Term)
  deriving (Show)

--------------------------------------------------------------------------------
-- | Monad

type InferM = ExceptT InferError (State InferState)

--------------------------------------------------------------------------------
-- |
--
inferM :: Term -> InferM Session
inferM term = do
  _ <- infer term (Crude Map.empty 0)
  -- return the final result
  gets stTarget

infer :: Term -> CrudeSession -> InferM Session
infer term@(C.Compose x _ p q _) (Crude pairs var) = do
  -- instantiate a new type variable, ignore the annotated type for the moment
  t <- freshType
  -- generate fresh context variables
  varP <- freshCtx
  varQ <- freshCtx
  -- refine the target
  refineTargetCtx var (Session Map.empty $ Set.fromList [varP, varQ])

  _ <- infer p (Crude (Map.insert x (Var t)        pairs) varP)
  _ <- infer q (Crude (Map.insert x (Var (dual t)) pairs) varQ)

  return $ Session pairs (Set.fromList [varP, varQ])



infer term@(C.Output x y p q _) (Crude pairs var) = do
  t <- getChannelTypeVar term x pairs
  -- instantiate some new type variables
  u <- freshType
  v <- freshType
  let newType = Times (Var u) (Var v)
  -- replace t with (Times u v)
  refineTargetType t newType
  -- generate fresh context variables
  varP <- freshCtx
  varQ <- freshCtx
  -- split the context
  refineTargetCtx var (Session Map.empty $ Set.fromList [varP, varQ])


  _ <- infer p (Crude (Map.insert y (Var u) pairs) varP)
  _ <- infer q (Crude (Map.insert x (Var v) pairs) varQ)

  return $ Session (Map.insert x newType pairs) (Set.fromList [varP, varQ])

infer term@(C.Input x y p _) (Crude context others) = do
  t <- getChannelTypeVar term x context
  -- instantiate some new type variables
  u <- freshType
  v <- freshType
  -- replace t with (Par u v)
  let newType = Par (Var u) (Var v)
  refineTargetType t newType

  _ <- infer p (Crude (Map.insert y (Var u) $ Map.insert x (Var t) context) others)

  return $ Session (Map.insert x newType context) (Set.fromList [others])

infer term@(C.EmptyOutput x _) (Crude context others) = do
  checkChannelsInContext term (Set.singleton x) context

  t <- getChannelTypeVar term x context
  -- replace t with One
  refineTargetType t One
  -- "others" be empty
  removeTargetCtx others

  return $ Session (Map.insert x One context) (Set.fromList [others])

infer term@(C.EmptyInput x p _) (Crude context others) = do
  checkChannelsInContext term (Set.singleton x) context

  t <- getChannelTypeVar term x context
  -- replace t with Bot
  refineTargetType t Bot

  return $ Session (Map.delete x context) (Set.fromList [others])

infer _ _ = undefined

checkChannelsInContext :: Term -> Set Chan -> Context -> InferM ()
checkChannelsInContext term channels context = do
  let notFound = Map.withoutKeys context channels
  unless (Map.null notFound) $
    throwError $ ChannelsNotInContext term (Map.keysSet notFound) context

removeTargetCtx :: CtxVar -> InferM ()
removeTargetCtx var = refineTargetCtx var (Session Map.empty Set.empty)

-- substitute all references to "var" with the provided context and ctx vars
refineTargetCtx :: CtxVar -> Session -> InferM ()
refineTargetCtx var (Session pairs' vars') = do
  Session _ vars <- gets stTarget
  when (Set.member var vars) $ do
    modifyPairs    (Map.union pairs')
    modifyContexts (Set.union vars' . Set.delete var)

-- substitute some type variable with some type
refineTargetType :: TypeVar -> Type -> InferM ()
refineTargetType var t = modifyPairs (Map.map (substitute var t))

-- replace a type variable in some type with another type
substitute :: TypeVar -> Type -> Type -> Type
substitute (Pos i) new (Var (Pos j)) = new
substitute (Pos i) new (Var (Neg j)) = dual new
substitute (Neg i) new (Var (Pos j)) = dual new
substitute (Neg i) new (Var (Neg j)) = new
substitute var new (Dual t) = Dual (substitute var new t)
substitute var new (Times t u) = Times (substitute var new t) (substitute var new u)
substitute var new (Par t u) = Par (substitute var new t) (substitute var new u)
substitute var new (Plus t u) = Plus (substitute var new t) (substitute var new u)
substitute var new (With t u) = With (substitute var new t) (substitute var new u)
substitute var new (Acc t) = Acc (substitute var new t)
substitute var new (Req t) = Req (substitute var new t)
substitute var new (Exists t u) = Exists t (substitute var new u)
substitute var new (Forall t u) = Forall t (substitute var new u)
substitute _ _ others = others

-- unify :: Type -> Type -> InferM ()
-- unify (Var i)  t        = modifyPairs (Map.map (substitute i t))
-- unify (Dual t) (Dual u) = unify t u
-- unify (Times t u) (Times v w) = unify t v >> unify u w
-- unify t u = throwError $ CannotUnify t u

-- -- there should be only one context variable before refining
-- checkContextVariableSize :: Set CtxVar -> InferM ()
-- checkContextVariableSize term vars =
--   when (Set.size vars > 1)
--     (throwError $ ToManyContextVariables term vars)

  -- modifyTarget $ \ Context ctx vars ->

getChannelTypeVar :: Term -> Chan -> Context -> InferM TypeVar
getChannelTypeVar term chan context = case Map.lookup chan context of
  Nothing -> do
    -- generate a fresh type variable
    t <- freshType
    modifyPairs (Map.insert chan (Var t))
    return t

    -- throwError $ ChannelNotInContext term chan ctx
  Just (Var i) -> return i
  Just t -> throwError $ ShouldBeTypeVar t
