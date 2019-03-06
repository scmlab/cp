module TypeChecking.Inference2 where

import qualified Syntax.Concrete as C
import Syntax.Concrete (HasDual(..))
import Syntax.TypeChecking

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
type TypeVar = Index

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

putTarget :: Session -> InferM ()
putTarget x = modify $ \ st -> st { stTarget = x }

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  -- | ToManyContextVariables Term (Set CtxVar)
  -- | VarNotAssumed Var
  | ChannelNotInContext Term Chan Context
  | ShouleBeTypeVar Type
  -- | OverlappedContext Session
  -- | ContextShouldBeTheSame Session Session
  -- | ContextShouldAllBeRequests Context
  -- | CannotUnify Type Type
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
  -- first we check that if the channel "x" is in the context
  t <- checkChannelInContext term x pairs
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

infer term@(C.EmptyOutput x _) (Crude pairs var) = do
  -- we check that if the channel "x" is in the context
  t <- checkChannelInContext term x pairs
  -- replace t with One
  refineTargetType t One

  return $ Session (Map.insert x One pairs) (Set.fromList [var])

infer _ _ = undefined

refineTargetCtx :: CtxVar   -- the context variable to be substituted
          -> Session
          -> InferM ()
refineTargetCtx var (Session pairs vars) = do
  -- substitute all references to "var" with the provided context and ctx vars
  Session pairs vars <- gets stTarget
  when (Set.member var vars) $ do
    putTarget $ Session
                  (Map.union pairs pairs)
                  (Set.union vars $ Set.delete var vars)

refineTargetType  :: TypeVar
                  -> Type
                  -> InferM ()
refineTargetType var t = do
  Session pairs vars <- gets stTarget
  let pairs' = Map.map (substitute var t) pairs
  putTarget $ Session pairs' vars





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


-- -- there should be only one context variable before refining
-- checkContextVariableSize :: Set CtxVar -> InferM ()
-- checkContextVariableSize term vars =
--   when (Set.size vars > 1)
--     (throwError $ ToManyContextVariables term vars)

  -- modifyTarget $ \ Context ctx vars ->

checkChannelInContext :: Term -> Chan -> Context -> InferM TypeVar
checkChannelInContext term chan ctx = case Map.lookup chan ctx of
  Nothing ->
    throwError $ ChannelNotInContext term chan ctx
  Just (Var i) -> return i
  Just t -> throwError $ ShouleBeTypeVar t

--
-- infer' :: Term -> Context -> InferM Context
-- infer' (C.Output x y p q _) ctx = do
--   Context m s <- infer' y p
--   Context n t <- infer' x q
--
--   -- unify ctx (Context (Map.fromList [(x, One)]) Nothing)
--
-- infer' (C.EmptyOutput x _) ctx = do
--   unify ctx (Context (Map.fromList [(x, One)]) Nothing)
-- infer' _ _ = undefined
--
-- data Target = Target
--                 (Map Var Type)  -- e : T, ....
--                 (Set CtxVar)    -- Γ, Δ, ...
--                 deriving (Show)
--
-- -- only one ore zero variable Γ is allowed
-- data Context = Context (Map Var Type) (Maybe CtxVar)
--              deriving (Show)
--
-- -- empty :: Context
-- -- empty = Context Map.empty Nothing
--
-- isEmpty :: Context -> Bool
-- isEmpty (Context _ (Just _)) = False
-- isEmpty (Context m Nothing) = Map.null m
--
-- -- insert :: Var -> Type -> Context -> Context
-- -- insert x t (Context m s) = Context (Map.insert x t m) s
-- --
-- -- add :: CtxVar -> Context -> Context
-- -- add var (Context m _) = Context m (Just var)
--
-- -- unifyType :: Type -> Type -> InferM Type
--
--
-- -- substitute the context of the target
-- substitute :: CtxVar -> Context -> InferM ()
-- substitute var ctx = do
--   target <- gets stTarget
--   putTarget (subst var ctx target)
--
--   where
--     -- substitute references to "var" in the latter context with the former context
--     subst :: CtxVar -> Context -> Target -> Target
--     subst v (Context m s) (Target n t) = if Set.member v t
--       then Target (Map.union m n) (case s of
--                                       Just s' -> (Set.insert s' $ Set.delete v t)
--                                       Nothing -> (Set.delete v t))
--       else Target n t
--
--   --   case viewContext ctx of
--   -- Empty      -> Context m s
--   -- NoPairs s' -> Context m (Set.update )
--
-- -- substitute var ctx = do
-- --   modify $ \ st -> st { stCtxMap = IntMap.update (\_ -> Just ctx) var (stCtxMap st) }
--
-- unify :: Context -> Context -> InferM Context
-- unify (Context m Nothing) (Context n Nothing)  = do
--   o <- unifyPairs m n
--   return $ Context o Nothing
-- unify (Context m Nothing) (Context n (Just t)) = undefined
-- unify (Context m (Just s)) (Context n Nothing)  = do
--   -- eliminate context variable s
--   substitute s (Context n Nothing)
--   --
--   o <- unifyPairs m n
--   return $ Context o Nothing
-- unify (Context m (Just s)) (Context n (Just t)) = undefined
--
-- unifyPairs :: Map Var Type -> Map Var Type -> InferM (Map Var Type)
-- unifyPairs a b = do
--   -- find out if the same variable appear in both maps
--   -- variables appear in both side should have the same type
--   let intersected = Map.elems $ Map.intersectionWith (,) a b
--   forM_ intersected $ \ (s, t) -> do
--     unless (s == t) $ throwError $ CannotUnify s t
--   -- after the previous step it should be safe to return the union of both maps
--   return $ Map.union a b
--
--
--   --
--   --   Map.merge
--   -- -- basically just union if one is missing
--   -- (Map.mapMaybeMissing (\_ x -> Just x))
--   -- (Map.mapMaybeMissing (\_ x -> Just x))
--   -- -- unify the type if exist in both maps
--   -- (Map.zipWithMaybeMatched (\_ x y -> if x == y then Just x else Nothing))
--
--
-- -- unify :: Context -> Context -> InferM Context
-- -- unify a b = case (viewContext a, viewContext b) of
-- --   -- nothing to do
-- --   (Empty         , Empty         ) -> return empty
-- --   -- throw if one is empty but the another has a pair
-- --   (Empty         , NoPairs _     ) -> throwError $ CannotUnifyContext a b
-- --   (Empty         , Everything _ _) -> throwError $ CannotUnifyContext a b
-- --   (NoPairs s    , Empty         ) -> do
-- --     forM_ s (flip substitute empty)
-- --     return empty
-- --   -- decree that s & t should both be of size 1
-- --   (NoPairs s     , NoPairs t     ) -> undefined
-- --     -- unless (Set.size s == 1 && Set.size t == 1) $
-- --     --    throwError $ CannotUnifyContext a b
-- --   (NoPairs _     , Everything _ _) -> return empty
-- --   (Everything _ _, Empty         ) -> return empty
-- --   (Everything _ _, NoPairs _     ) -> return empty
-- --   (Everything _ _, Everything _ _) -> return empty
--
-- --
-- -- data ContextView  = Empty
-- --                   | NoPairs (Set CtxVar)
-- --                   | Everything (Map Var Type) (Set CtxVar)
-- --                   deriving (Show)
-- --
-- -- viewContext :: Context -> ContextView
-- -- viewContext (Context m s)
-- --   |      Map.null m  &&      Set.null s  = Empty
-- --   |      Map.null m  && not (Set.null s) = NoPairs s
-- --   -- | not (Map.null m) &&     (Set.null s) = PairsOnly m
-- --   | otherwise                            = Everything m s
--
--
-- -- unify :: Context -> Context -> InferM Context
-- -- unify (Has x t a) (Has y u b)   = if x == y
-- --                                     then do
-- --                                       v <- unifyType t u
-- --                                       c <- unifyType a b
-- --                                       return $ Has x v c
-- --                                     else do
-- --
-- -- unify (Has x t a) (Some b)      = Empty
-- -- unify (Has x t a) Empty         = throwError CannotUnify2
-- -- unify (Some a)    (Has x t b)   = Empty
-- -- unify (Some a)    (Some b)      = Some (unify a b)
-- -- unify (Some a)    Empty         = Empty
-- -- unify Empty       (Has x t a)   = throwError CannotUnify2
-- -- unify Empty       (Some a)      = Empty
-- -- unify Empty       Empty         = Empty
--
--
--
-- -- data Judgement = Judgement Term Ctx
-- --   deriving (Show)
--
-- -- -- ruleAx :: Judgement
-- --
-- -- -- unify ::
-- --
-- --
-- -- infer' :: Judgement -> InferM Judgement
-- -- -- infer' (Judgement (C.Link w x _) ctx) = ctx
-- -- infer' (Judgement (C.EmptyOutput x _) ctx) = do
-- --   unify ctx (Has x One Empty)
-- --   return Done
-- -- infer' _ = undefined
