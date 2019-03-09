module TypeChecking.Inference where

import qualified Syntax.Concrete as C
import Syntax.Abstract (Type(..))
import Syntax.Base

import Prelude hiding (lookup)
-- import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Bifunctor
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

import Debug.Trace

--------------------------------------------------------------------------------
-- | State

type Chan = C.TermName Loc
type Term = C.Process Loc

type CtxVar = Int

data Alignment = Aligned | Undecided (Set CtxVar)
  deriving (Show)
type Session = Map Chan (Alignment, Type)
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

pushMany :: [Judgement] -> InferM ()
pushMany = mapM_ push

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  -- | ToManyContextVariables Term (Set CtxVar)
  -- | VarNotAssumed Var
  | ChannelsNotInContext Term (Set Chan) Session
  -- | ChannelNotUsed Context
  | ShouldBeTypeVar Term Type
  -- | OverlappedContext Session
  -- | ContextShouldBeTheSame Session Session
  -- | ContextShouldAllBeRequests Context
  | CannotUnify Type Type
  | ToManyContextVariablesToStartWith Term (Set CtxVar)
  | NoContextVariableToStartWith Term
  | NoContextVariableForRefining Term
  | CannotAlignChannel Term Chan (Set CtxVar) (Set CtxVar)
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
    Just j  -> do
      step j >>= pushMany
      run
    Nothing -> do
      Judgement _ s _ <- gets stResult
      return s

step :: Judgement -> InferM [Judgement]
step judgement@(Judgement term session ctxs) = case term of

  C.Link x y _ -> do

    (varX, session', ctx) <- extractChannel judgement x
    let judgement' = Judgement term (Map.insert x (Aligned, varX) session') (Set.singleton ctx)
    (varY, session'', ctx') <- extractChannel judgement' y

    traceShow session'' $ return ()

    _ <- unify varX (dual varY)
    -- mark context variables empty
    refineContext ctx' Map.empty Set.empty
    return []



  C.Compose x _ p q _ -> do

    t <- freshType

    ctx <- extractCtxVar term ctxs


    -- split context
    ctxP <- freshCtx
    ctxQ <- freshCtx
    let ctx' = Set.fromList [ctxP, ctxQ]
    refineContext ctx Map.empty ctx'
    let sessionP = markAllUndecided ctx' session
    let sessionQ = markAllUndecided ctx' session


    return
      [ Judgement q (Map.insert x (Aligned, dual (Var t)) sessionP) (Set.singleton ctxQ)
      , Judgement p (Map.insert x (Aligned,       Var t ) sessionQ) (Set.singleton ctxP)
      ]


  C.Output x y p q _ -> do

    (var, session', ctx) <- extractChannel judgement x


    -- form new type
    a <- freshType
    b <- freshType
    let t = Times (Var a) (Var b)
    _ <- unify var t

    -- split context
    ctxP <- freshCtx
    ctxQ <- freshCtx
    let ctx' = Set.fromList [ctxP, ctxQ]
    refineContext ctx Map.empty ctx'
    let sessionP = markAllUndecided ctx' session'
    let sessionQ = markAllUndecided ctx' session'


    return
      [ Judgement q (Map.insert x (Aligned, Var b) sessionP) (Set.singleton ctxQ)
      , Judgement p (Map.insert y (Aligned, Var a) sessionQ) (Set.singleton ctxP)
      ]

  C.Input x y p _ -> do

    (var, session', ctx) <- extractChannel judgement x


    -- form new type
    a <- freshType
    b <- freshType
    let t = Par (Var a) (Var b)

    -- refine
    _ <- unify var t

    let session'' = Map.insert y (Aligned, Var a)
                    $ Map.insert x (Aligned, Var b) session'
    return
      [ Judgement p session'' (Set.singleton ctx)
      ]

  C.EmptyOutput x _ -> do

    (var, _, ctx) <- extractChannel judgement x
    _ <- unify var One
    -- mark context variables empty
    refineContext ctx Map.empty Set.empty
    return []

  C.EmptyInput x p _ -> do

    (var, session', ctx) <- extractChannel judgement x
    _ <- unify var Bot

    return
      [ Judgement p session' (Set.singleton ctx)
      ]


  _ -> undefined

extractChannel :: Judgement -> Chan -> InferM (Type, Session, CtxVar)
extractChannel (Judgement term session ctxs) x = do
  case Map.lookup x session of
    -- "x" occured free
    Nothing      -> do
      ctx <- extractCtxVar term ctxs
      -- instantiate a new type variable and context variable
      var <- freshType
      ctx' <- freshCtx
      -- replace "ctx" with "var" + "ctx'"
      refineContext ctx (Map.fromList [(x, (Aligned, Var var))]) (Set.singleton ctx')

      return (Var var, session, ctx')

    -- "x" found, aligned
    Just (Aligned, t) -> do
      ctx <- extractCtxVar term ctxs
      let session' = Map.delete x session
      return (t, session', ctx)

    -- "x" found, alignment undecided
    Just (Undecided factions, t) -> do
      ctx <- extractCtxVar term ctxs
      if Set.member ctx factions
        then do
          refineAlignment x factions ctx
          let session' = Map.delete x session
          return (t, session', ctx)
        else throwError $ CannotAlignChannel term x factions ctxs

refineAlignment :: Chan -> Set CtxVar -> CtxVar -> InferM ()
refineAlignment chan ctxs ctx = do
  let refine (Judgement term ss cs) =
        if cs == ctxs
          then Judgement term (markAligned chan ss) cs
          else Judgement term ss cs
  -- refine the result session
  modifyResult refine
  -- refine the frontier stack
  modifyFrontier (map refine)

markAligned :: Chan -> Session -> Session
markAligned = Map.adjust (\(_,t) -> (Aligned, t))

markAllUndecided :: Set CtxVar -> Session -> Session
markAllUndecided ctxs = Map.map mark
  where
    mark (Undecided a, t) = (Undecided a, t)
    mark (Aligned, t) = (Undecided ctxs, t)

    -- do
    --   when (t /= One) $ throwError $ CannotUnify t One
    --
    --   -- eliminate all contexts
    --   forM_ (Set.toList ctxs) $ \ var -> do
    --     refineContext var Map.empty Set.empty

extractCtxVar :: Term -> Set CtxVar -> InferM CtxVar
extractCtxVar term ctxs = case Set.size ctxs of
  0 -> throwError $ NoContextVariableToStartWith term
  1 -> return     $ Set.findMin ctxs
  _ -> throwError $ ToManyContextVariablesToStartWith term ctxs

-- substitute all references to "var" with the provided session and ctx vars
refineContext :: CtxVar -> Session -> Set CtxVar -> InferM ()
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
  let refine (Judgement term ss cs) =
        Judgement term (Map.map (second (substitute var t)) ss) cs
  -- refine the result session
  modifyResult refine
  -- refine the frontier stack
  modifyFrontier (map refine)

-- replace a type variable in some type with another type
substitute :: TypeVar -> Type -> Type -> Type
substitute var new (Var var')
  | var ==      var' = new
  | var == dual var' = dual new
  | otherwise        = Var var'
substitute var new (Dual t)     = Dual (substitute var new t)
substitute var new (Times t u)  = Times (substitute var new t) (substitute var new u)
substitute var new (Par t u)    = Par (substitute var new t) (substitute var new u)
substitute var new (Plus t u)   = Plus (substitute var new t) (substitute var new u)
substitute var new (With t u)   = With (substitute var new t) (substitute var new u)
substitute var new (Acc t)      = Acc (substitute var new t)
substitute var new (Req t)      = Req (substitute var new t)
substitute var new (Exists t u) = Exists t (substitute var new u)
substitute var new (Forall t u) = Forall t (substitute var new u)
substitute _   _   others       = others

unify :: Type -> Type -> InferM Type
unify (Var    i  ) v            = refineType i v >> return v
unify t            (Var    j  ) = refineType j t >> return t
unify (Dual   t  ) v            = unify t        (dual v)
unify t            (Dual   v  ) = unify (dual t) v
unify (Times  t u) (Times  v w) = Times  <$> unify t v <*> unify u w
unify (Par    t u) (Par    v w) = Par    <$> unify t v <*> unify u w
unify (Plus   t u) (Plus   v w) = Plus   <$> unify t v <*> unify u w
unify (With   t u) (With   v w) = With   <$> unify t v <*> unify u w
unify (Acc    t  ) (Acc    v  ) = unify t v
unify (Req    t  ) (Req    v  ) = unify t v
unify (Exists _ u) (Exists _ w) = unify u w
unify (Forall _ u) (Forall _ w) = unify u w
unify One          One          = return One
unify Top          Top          = return Top
unify Zero         Zero         = return Zero
unify Bot          Bot          = return Bot
unify t            v            = throwError $ CannotUnify t v




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
