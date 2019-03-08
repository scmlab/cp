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
type Session = Map Chan Type
-- data CrudeSession = Crude Context CtxVar
-- data Session = Session Context (Set CtxVar)
--   deriving (Show)
data Judgement = Judgement Term Session (Set CtxVar)
  deriving (Show)

data InferState = InferState
  { stResult    :: Judgement
  , stFrontier  :: [Judgement]      -- stack of Judgements, frontier of DFS
  , stTypeCount :: Int              -- for type variables
  , stCtxCount  :: Int              -- for context variables
  } deriving (Show)

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

modifyResult :: (Judgement -> Judgement) -> InferM ()
modifyResult f = modify $ \ st -> st { stResult = f (stResult st) }

modifyFrontier :: ([Judgement] -> [Judgement]) -> InferM ()
modifyFrontier f = modify $ \ st -> st { stFrontier = f (stFrontier st) }

modifyFrontierM :: ([Judgement] -> InferM [Judgement]) -> InferM ()
modifyFrontierM f = do
  stack <- gets stFrontier
  stack' <- f stack
  modify $ \ st -> st { stFrontier = stack' }

pop :: InferM (Maybe Judgement)
pop = do
  stack <- gets stFrontier
  case stack of
    [] -> return Nothing
    (x:xs) -> do
      modifyFrontier (const xs)
      return (Just x)

push :: Judgement -> InferM ()
push judgement = modifyFrontier (\ stack -> judgement : stack)


-- initialize :: Term -> Infer ()
-- initialize term = do
-- modify $ \ state -> state
--   { stFrontier = [Judgement term Map.empty (Set.singleton 0)]
--   }

-- modifyPairs :: (Context -> Context) -> InferM ()
-- modifyPairs f = do
--   Session pairs contexts <- gets stTarget
--   modify $ \ st -> st { stTarget = Session (f pairs) contexts }
--
-- modifyContexts :: (Set CtxVar -> Set CtxVar) -> InferM ()
-- modifyContexts f = do
--   Session pairs contexts <- gets stTarget
--   modify $ \ st -> st { stTarget = Session pairs (f contexts) }

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  -- | ToManyContextVariables Term (Set CtxVar)
  -- | VarNotAssumed Var
  | ChannelsNotInContext Term (Set Chan) Session
  -- | ChannelNotUsed Context
  | ShouldBeTypeVar Type
  -- | OverlappedContext Session
  -- | ContextShouldBeTheSame Session Session
  -- | ContextShouldAllBeRequests Context
  | CannotUnify Type Type
  | NoContextVariableForRefining Term
  -- | CannotUnifyContext Context Context
--   | VarNotFresh Var (Maybe Term)
  deriving (Show)

--------------------------------------------------------------------------------
-- | Monad

type InferM = ExceptT InferError (State InferState)

--------------------------------------------------------------------------------
-- |
--

infer :: Term -> Either InferError Session
infer term = evalState (runExceptT run) initState
  where
    initJudgement = Judgement term Map.empty (Set.singleton 0)
    initState = InferState initJudgement [initJudgement] 0 1

-- keep running until the frontier stack is empty
run :: InferM Session
run = do
  result <- pop
  case result of
    Just j  -> step j >> run
    Nothing -> do
      Judgement _ s _ <- gets stResult
      return s

step :: Judgement -> InferM ()
step (Judgement term@(C.Output x y p q _) session ctxs) =
  undefined
  -- case Map.lookup x session of
  --   -- "x" occured free
  --   Nothing      -> do
  --     t <- freshType
  --
  --     -- take one of the context variable and refine it with the generated pair
  --     case Set.lookupMin ctxs of
  --       Nothing -> throwError $ NoContextVariableForRefining term
  --       Just ctx -> do
  --         ctx' <- freshCtx
  --         refineContext ctx (Map.fromList [(x, Var t)]) (Set.singleton ctx')
  --
  --   -- "x" is some type variable, ready to be refined
  --   Just (Var v) ->
  --     refineType v One
  --   -- "x" is some other type
  --   Just t       -> when (t /= One) $ throwError $ CannotUnify t One

step (Judgement term@(C.EmptyOutput x _) session ctxs) = do
  case Map.lookup x session of
    -- "x" occured free
    Nothing      -> do
      t <- freshType

      -- take one of the context variable and refine it with the generated pair
      case Set.lookupMin ctxs of
        Nothing -> throwError $ NoContextVariableForRefining term
        Just ctx -> do
          ctx' <- freshCtx
          refineContext ctx (Map.fromList [(x, Var t)]) (Set.singleton ctx')

          -- eliminate all contexts
          forM_ (Set.toList ctxs) $ \ var -> do
            refineContext var Map.empty Set.empty

    -- "x" is some type variable, ready to be refined
    Just (Var v) -> do
      refineType v One

      -- eliminate all contexts
      forM_ (Set.toList ctxs) $ \ var -> do
        refineContext var Map.empty Set.empty

    -- "x" is some other type
    Just t       -> do
      when (t /= One) $ throwError $ CannotUnify t One

      -- eliminate all contexts
      forM_ (Set.toList ctxs) $ \ var -> do
        refineContext var Map.empty Set.empty


step (Judgement _ _ _) = undefined


-- substitute all references to "var" with the provided session and ctx vars
refineContext :: CtxVar -> Session -> (Set CtxVar) -> InferM ()
refineContext var session ctxs = do
  -- refine the result session
  modifyResult $ \ (Judgement term ss cs) ->
    if Set.member var cs
      then Judgement term
              (Map.union ss session)
              (Set.union cs (Set.delete var ctxs))
      else Judgement term ss cs

  -- refine the frontier stack
  modifyFrontier $ map $ \ (Judgement term ss cs) ->
    if Set.member var cs
      then Judgement term
              (Map.union ss session)
              (Set.union cs (Set.delete var ctxs))
      else Judgement term ss cs

refineType :: TypeVar -> Type -> InferM ()
refineType var t = do
  -- refine the result session
  modifyResult $ \ (Judgement term ss cs) ->
    Judgement term (Map.map (substitute var t) ss) cs
  -- refine the frontier stack
  modifyFrontier $ map $ \ (Judgement term ss cs) ->
    Judgement term (Map.map (substitute var t) ss) cs

-- replace a type variable in some type with another type
substitute :: TypeVar -> Type -> Type -> Type
substitute var new (Var var')
  | var ==      var' = new
  | var == dual var' = dual new
  | otherwise        = Var var'
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






--
-- runInferM :: InferM a -> (Either InferError a, InferState)
-- runInferM program = runState (runExceptT program) initInferState



-- inferM :: Term -> InferM Session
-- inferM term = undefined
--   v <- freshCtx
--   let judgement = Judgement term Map.empty (Set.singleton v)
--   push judgement
--
--   run
--
--   where
--     run = do
--       result <- infer
--       case result of
--         Just session -> return session
--         Nothing -> run
--
--
-- --   _ <- infer term (Crude Map.empty 0)
-- --   -- return the final result
-- --   gets stTarget
-- --
-- infer :: InferM (Maybe Session)
-- infer = return Nothing



-- infer
-- infer term@(C.Compose x _ p q _) (Crude pairs var) = do
--
--   -- instantiate a new type variable, ignore the annotated type for the moment
--   t <- freshType
--   -- generate fresh context variables
--   varP <- freshCtx
--   varQ <- freshCtx
--   -- refine the target
--   refineTargetCtx var (Session Map.empty $ Set.fromList [varP, varQ])
--
--   infer p (Crude (Map.insert x (Var t)        pairs) varP)
--
--   _ <- infer q (Crude (Map.insert x (Var (dual t)) pairs) varQ)
--
--   return $ Session pairs (Set.fromList [varP, varQ])
--
--
--
-- infer term@(C.Output x y p q _) (Crude context others) = do
--   t <- getChannelTypeVar term x context
--   -- instantiate some new type variables
--   u <- freshType
--   v <- freshType
--   let newType = Times (Var u) (Var v)
--
--
--
--   -- replace t with (Times u v)
--   refineTargetType t newType
--   -- generate fresh context variables
--   varP <- freshCtx
--   varQ <- freshCtx
--   -- split the context
--   refineTargetCtx others (Session Map.empty $ Set.fromList [varP, varQ])
--
--   let context' = Map.delete x context
--
--
--
--   _ <- infer p (Crude (Map.insert y (Var u) context') varP)
--   _ <- infer q (Crude (Map.insert x (Var v) context') varQ)
--
--   return $ Session (Map.insert x newType context') (Set.fromList [varP, varQ])
--
-- infer term@(C.Input x y p _) (Crude context others) = do
--   t <- getChannelTypeVar term x context
--   let context' = Map.delete x context
--
--   -- instantiate some new type variables
--   u <- freshType
--   v <- freshType
--   -- replace t with (Par u v)
--   let newType = Par (Var u) (Var v)
--   refineTargetType t newType
--
--   infer p (Crude (Map.insert y (Var u) $ Map.insert x (Var t) context') others)
--
--
--   return $ Session (Map.insert x newType context') (Set.fromList [others])
--
-- infer term@(C.EmptyOutput x _) (Crude context others) = do
--   checkChannelsInContext term (Set.singleton x) context
--
--   t <- getChannelTypeVar term x context
--   -- replace t with One
--   refineTargetType t One
--   -- "others" be empty
--   removeTargetCtx others
--
--   return $ Session (Map.insert x One context) (Set.fromList [others])
--
-- infer term@(C.EmptyInput x p _) (Crude context others) = do
--   checkChannelsInContext term (Set.singleton x) context
--
--   t <- getChannelTypeVar term x context
--   -- replace t with Bot
--   refineTargetType t Bot
--
--   return $ Session (Map.delete x context) (Set.fromList [others])
--
-- infer _ _ = undefined
--
-- checkChannelsInContext :: Term -> Set Chan -> Context -> InferM ()
-- checkChannelsInContext term channels context = do
--   let notFound = Map.withoutKeys context channels
--   unless (Map.null notFound) $
--     throwError $ ChannelsNotInContext term (Map.keysSet notFound) context
--
-- removeTargetCtx :: CtxVar -> InferM ()
-- removeTargetCtx var = refineTargetCtx var (Session Map.empty Set.empty)
--
-- -- substitute all references to "var" with the provided context and ctx vars
-- refineTargetCtx :: CtxVar -> Session -> InferM ()
-- refineTargetCtx var (Session pairs' vars') = do
--   Session _ vars <- gets stTarget
--   when (Set.member var vars) $ do
--     modifyPairs    (Map.union pairs')
--     modifyContexts (Set.union vars' . Set.delete var)
--
-- -- unify :: Type -> Type -> InferM ()
-- -- unify (Var i)  t        = modifyPairs (Map.map (substitute i t))
-- -- unify (Dual t) (Dual u) = unify t u
-- -- unify (Times t u) (Times v w) = unify t v >> unify u w
-- -- unify t u = throwError $ CannotUnify t u
--
-- -- -- there should be only one context variable before refining
-- -- checkContextVariableSize :: Set CtxVar -> InferM ()
-- -- checkContextVariableSize term vars =
-- --   when (Set.size vars > 1)
-- --     (throwError $ ToManyContextVariables term vars)
--
--   -- modifyTarget $ \ Context ctx vars ->
--
-- getChannelTypeVar :: Term -> Chan -> Context -> InferM TypeVar
-- getChannelTypeVar term chan context = case Map.lookup chan context of
--   Nothing -> do
--     -- generate a fresh type variable
--     t <- freshType
--     modifyPairs (Map.insert chan (Var t))
--     return t
--
--     -- throwError $ ChannelNotInContext term chan ctx
--   Just (Var i) -> return i
--   Just t -> throwError $ ShouldBeTypeVar t
