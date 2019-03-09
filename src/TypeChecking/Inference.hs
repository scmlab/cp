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

type Session = Map Chan Type
-- data Alignment = Aligned | Undecided (Set CtxVar)
--   deriving (Show)
-- data Judgement = Judgement Term Session (Set CtxVar)
--   deriving (Show)

data InferState = InferState
  {
  --   stResult    :: Judgement
  -- , stFrontier  :: [Judgement]      -- stack of Judgements, frontier of DFS
    stTypeCount :: Int              -- for type variables
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

-- modifyResult :: (Judgement -> Judgement) -> InferM ()
-- modifyResult f = modify $ \ st -> st { stResult = f (stResult st) }
--
-- modifyFrontier :: ([Judgement] -> [Judgement]) -> InferM ()
-- modifyFrontier f = modify $ \ st -> st { stFrontier = f (stFrontier st) }
--
-- modifyFrontierM :: ([Judgement] -> InferM [Judgement]) -> InferM ()
-- modifyFrontierM f = do
--   stack <- gets stFrontier
--   stack' <- f stack
--   modify $ \ st -> st { stFrontier = stack' }
--
-- pop :: InferM (Maybe Judgement)
-- pop = do
--   stack <- gets stFrontier
--   case stack of
--     [] -> return Nothing
--     (x:xs) -> do
--       modifyFrontier (const xs)
--       return (Just x)
--
-- push :: Judgement -> InferM ()
-- push judgement = modifyFrontier (\ stack -> judgement : stack)
--
-- pushMany :: [Judgement] -> InferM ()
-- pushMany = mapM_ push

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  -- | ChannelsNotInContext Term (Set Chan) Session
  -- | ShouldBeTypeVar Term Type
  | CannotUnify Term Type Type Type Type
  | ContextShouldBeAllRequesting Term Session
  | CannotAppearInside Term Chan
  -- | NoContextVariableForRefining Term
  -- | CannotAlignChannel Term Chan (Set CtxVar) (Set CtxVar)
  | ChannelNotComsumed Term Session
  deriving (Show)

--------------------------------------------------------------------------------
-- | Monad

type InferM = ExceptT InferError (State InferState)

--------------------------------------------------------------------------------
-- |
--

inferTerm :: Term -> Either InferError Session
inferTerm term = evalState (runExceptT (infer term Map.empty)) initState
  where
    initState = InferState 0 1

infer :: Term -> Session -> InferM Session
infer term session = case term of
  C.Link x y _ -> do

    (a, session') <- extractChannel x session
    (b, session'') <- extractChannel y session'

    unless (Map.null session'') $
      throwError $ ChannelNotComsumed term session''


    (t, session''') <- unifyAndSubstitute term a (dual b) session''

    return
      $ Map.insert x t
      $ Map.insert y (dual t)
      $ session'''

  C.Compose x _ p q _ -> do

    (t, sessionP) <- infer p session >>= extractChannel x

    -- splitting the context
    let session' = Map.difference session sessionP
    (u, sessionQ) <- infer q session' >>= extractChannel x


    let session'' = Map.union sessionP sessionQ
    (v, session''') <- unifyAndSubstitute term t (dual u) session''

    return session'''

  C.Output x y p q _ -> do

    (a, sessionP) <- infer p session >>= extractChannel y

    let session' = Map.difference session sessionP
    (b, sessionQ) <- infer q session' >>= extractChannel x

    let session'' = Map.union sessionP sessionQ
    let t = Times a b
    return
      $ Map.insert x t
      $ session''

  C.Input x y p _ -> do

    traceShow session $ return ()

    (a, session') <- infer p session >>= extractChannel y
    (b, session'') <- extractChannel x session'

    let t = Par a b
    return
      $ Map.insert x t
      $ session''

  C.Accept x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y
    checkContextWhenAccept term session'

    if Map.member x session'
      then throwError $ CannotAppearInside p x
      else return
            $ Map.insert x (Acc a)
            $ session'

  C.Request x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y

    if Map.member x session'
      then throwError $ CannotAppearInside p x
      else return
            $ Map.insert x (Req a)
            $ session'

    -- error $ show (a, session')

  C.EmptyOutput x _ -> do

    (a, leftover) <- extractChannel x session
    unless (Map.null leftover) $
      throwError $ ChannelNotComsumed term leftover

    return $ Map.fromList
      [ (x, One)
      ]

  C.EmptyInput x p _ -> do

    (t, session') <- extractChannel x session
    (t', session'') <- unifyAndSubstitute term t Bot session'

    session''' <- infer p session''

    if Map.member x session'''
      then throwError $ CannotAppearInside p x
      else return
            $ Map.insert x t'
            $ session'''


  _ -> undefined

extractChannel :: Chan -> Session -> InferM (Type, Session)
extractChannel chan session = do
  case Map.lookup chan session of
    Nothing -> do
      t <- freshType
      return (Var t, session)
    Just t -> do
      let session' = Map.delete chan session
      return (t, session')

unifyAndSubstitute :: Term -> Type -> Type -> Session -> InferM (Type, Session)
unifyAndSubstitute term a b session = do
    let (result, subst) = unify a b
    case result of
      Left (t, u) -> throwError $ CannotUnify term a b t u
      Right t -> do
        let session' = execSubstituton session subst
        return (t, session')

-- let (result, state) = runState (runExceptT (run a b)) []
-- case result of
--   Left (t, u) -> throwError $ CannotUnify t u
--   Right t -> return (t, state)
    where
      execSubstituton :: Session -> [Substitution] -> Session
      execSubstituton = foldr $ \ (Substitute var t) -> Map.map (substitute var t)

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

--------------------------------------------------------------------------------
-- | Unification

data Substitution = Substitute TypeVar Type
  deriving (Show)
type UniError = (Type, Type)
type UniM = ExceptT UniError (State [Substitution])

unify :: Type -> Type -> (Either (Type, Type) Type, [Substitution])
unify a b = runState (runExceptT (run a b)) []
  where
    run :: Type -> Type -> UniM Type
    run (Var    i  ) v            = do
      modify ((:) (Substitute i v))
      return v
    run t            (Var    j  ) = do
      modify ((:) (Substitute j t))
      return t
    run (Dual   t  ) v            = run t        (dual v)
    run t            (Dual   v  ) = run (dual t) v
    run (Times  t u) (Times  v w) = Times  <$> run t v <*> run u w
    run (Par    t u) (Par    v w) = Par    <$> run t v <*> run u w
    run (Plus   t u) (Plus   v w) = Plus   <$> run t v <*> run u w
    run (With   t u) (With   v w) = With   <$> run t v <*> run u w
    run (Acc    t  ) (Acc    v  ) = run t v
    run (Req    t  ) (Req    v  ) = run t v
    run (Exists _ u) (Exists _ w) = run u w
    run (Forall _ u) (Forall _ w) = run u w
    run One          One          = return One
    run Top          Top          = return Top
    run Zero         Zero         = return Zero
    run Bot          Bot          = return Bot
    run t            v            = throwError (t, v)

-- all channels should be requesting something
checkContextWhenAccept :: Term -> Session -> InferM ()
checkContextWhenAccept term session = do
  let result = List.all requesting $ Map.toList session
  unless result $
    throwError $ ContextShouldBeAllRequesting term session

  where
    requesting :: (Chan, Type) -> Bool
    requesting (_, Acc _) = True
    requesting (_,     _) = False

--
-- -- keep running until the frontier stack is empty
-- run :: InferM Session
-- run = do
--   result <- pop
--   case result of
--     Just j  -> do
--       step j >>= pushMany
--       run
--     Nothing -> do
--       Judgement _ s _ <- gets stResult
--       return s
--
-- step :: Judgement -> InferM [Judgement]
-- step judgement@(Judgement term session ctxs) = case term of
--
--   C.Link x y _ -> do
--
--     (varX, session', ctx) <- extractChannel judgement x
--     let judgement' = Judgement term (Map.insert x (Aligned, varX) session') (Set.singleton ctx)
--     (varY, session'', ctx') <- extractChannel judgement' y
--
--     traceShow session'' $ return ()
--
--     _ <- unify varX (dual varY)
--     -- mark context variables empty
--     refineContext ctx' Map.empty Set.empty
--     return []
--
--
--
--   C.Compose x _ p q _ -> do
--
--     t <- freshType
--
--     ctx <- extractCtxVar term ctxs
--
--
--     -- split context
--     ctxP <- freshCtx
--     ctxQ <- freshCtx
--     let ctx' = Set.fromList [ctxP, ctxQ]
--     refineContext ctx Map.empty ctx'
--     let sessionP = markAllUndecided ctx' session
--     let sessionQ = markAllUndecided ctx' session
--
--
--     return
--       [ Judgement q (Map.insert x (Aligned, dual (Var t)) sessionP) (Set.singleton ctxQ)
--       , Judgement p (Map.insert x (Aligned,       Var t ) sessionQ) (Set.singleton ctxP)
--       ]
--
--
--   C.Output x y p q _ -> do
--
--     (var, session', ctx) <- extractChannel judgement x
--
--
--     -- form new type
--     a <- freshType
--     b <- freshType
--     let t = Times (Var a) (Var b)
--     _ <- unify var t
--
--     -- split context
--     ctxP <- freshCtx
--     ctxQ <- freshCtx
--     let ctx' = Set.fromList [ctxP, ctxQ]
--     refineContext ctx Map.empty ctx'
--     let sessionP = markAllUndecided ctx' session'
--     let sessionQ = markAllUndecided ctx' session'
--
--
--     return
--       [ Judgement q (Map.insert x (Aligned, Var b) sessionP) (Set.singleton ctxQ)
--       , Judgement p (Map.insert y (Aligned, Var a) sessionQ) (Set.singleton ctxP)
--       ]
--
--   C.Input x y p _ -> do
--
--     (var, session', ctx) <- extractChannel judgement x
--
--
--     -- form new type
--     a <- freshType
--     b <- freshType
--     let t = Par (Var a) (Var b)
--
--     -- refine
--     _ <- unify var t
--
--     let session'' = Map.insert y (Aligned, Var a)
--                     $ Map.insert x (Aligned, Var b) session'
--     return
--       [ Judgement p session'' (Set.singleton ctx)
--       ]
--
--   C.EmptyOutput x _ -> do
--
--     (var, _, ctx) <- extractChannel judgement x
--     _ <- unify var One
--     -- mark context variables empty
--     refineContext ctx Map.empty Set.empty
--     return []
--
--   C.EmptyInput x p _ -> do
--
--     (var, session', ctx) <- extractChannel judgement x
--     _ <- unify var Bot
--
--     return
--       [ Judgement p session' (Set.singleton ctx)
--       ]
--
--
--   _ -> undefined
--
-- extractChannel :: Judgement -> Chan -> InferM (Type, Session, CtxVar)
-- extractChannel (Judgement term session ctxs) x = do
--   case Map.lookup x session of
--     -- "x" occured free
--     Nothing      -> do
--       ctx <- extractCtxVar term ctxs
--       -- instantiate a new type variable and context variable
--       var <- freshType
--       ctx' <- freshCtx
--       -- replace "ctx" with "var" + "ctx'"
--       refineContext ctx (Map.fromList [(x, (Aligned, Var var))]) (Set.singleton ctx')
--
--       return (Var var, session, ctx')
--
--     -- "x" found, aligned
--     Just (Aligned, t) -> do
--       ctx <- extractCtxVar term ctxs
--       let session' = Map.delete x session
--       return (t, session', ctx)
--
--     -- "x" found, alignment undecided
--     Just (Undecided factions, t) -> do
--       ctx <- extractCtxVar term ctxs
--       if Set.member ctx factions
--         then do
--           refineAlignment x factions ctx
--           let session' = Map.delete x session
--           return (t, session', ctx)
--         else throwError $ CannotAlignChannel term x factions ctxs
--
-- refineAlignment :: Chan -> Set CtxVar -> CtxVar -> InferM ()
-- refineAlignment chan ctxs ctx = do
--   let refine (Judgement term ss cs) =
--         if cs == ctxs
--           then Judgement term (markAligned chan ss) cs
--           else Judgement term ss cs
--   -- refine the result session
--   modifyResult refine
--   -- refine the frontier stack
--   modifyFrontier (map refine)
--
-- markAligned :: Chan -> Session -> Session
-- markAligned = Map.adjust (\(_,t) -> (Aligned, t))
--
-- markAllUndecided :: Set CtxVar -> Session -> Session
-- markAllUndecided ctxs = Map.map mark
--   where
--     mark (Undecided a, t) = (Undecided a, t)
--     mark (Aligned, t) = (Undecided ctxs, t)
--
--     -- do
--     --   when (t /= One) $ throwError $ CannotUnify t One
--     --
--     --   -- eliminate all contexts
--     --   forM_ (Set.toList ctxs) $ \ var -> do
--     --     refineContext var Map.empty Set.empty
--
-- extractCtxVar :: Term -> Set CtxVar -> InferM CtxVar
-- extractCtxVar term ctxs = case Set.size ctxs of
--   0 -> throwError $ NoContextVariableToStartWith term
--   1 -> return     $ Set.findMin ctxs
--   _ -> throwError $ ToManyContextVariablesToStartWith term ctxs
--
-- -- substitute all references to "var" with the provided session and ctx vars
-- refineContext :: CtxVar -> Session -> Set CtxVar -> InferM ()
-- refineContext var session ctxs = do
--   -- refine the result session
--   modifyResult $ \ (Judgement term ss cs) ->
--     if Set.member var cs
--       then Judgement term
--               (Map.union ss session)
--               (Set.union cs (Set.delete var ctxs))
--       else Judgement term ss cs
--
--   -- refine the frontier stack
--   modifyFrontier $ map $ \ (Judgement term ss cs) ->
--     if Set.member var cs
--       then Judgement term
--               (Map.union ss session)
--               (Set.union cs (Set.delete var ctxs))
--       else Judgement term ss cs
--
-- refineType :: TypeVar -> Type -> InferM ()
-- refineType var t = do
--   let refine (Judgement term ss cs) =
--         Judgement term (Map.map (second (substitute var t)) ss) cs
--   -- refine the result session
--   modifyResult refine
--   -- refine the frontier stack
--   modifyFrontier (map refine)
--
-- -- replace a type variable in some type with another type
-- substitute :: TypeVar -> Type -> Type -> Type
-- substitute var new (Var var')
--   | var ==      var' = new
--   | var == dual var' = dual new
--   | otherwise        = Var var'
-- substitute var new (Dual t)     = Dual (substitute var new t)
-- substitute var new (Times t u)  = Times (substitute var new t) (substitute var new u)
-- substitute var new (Par t u)    = Par (substitute var new t) (substitute var new u)
-- substitute var new (Plus t u)   = Plus (substitute var new t) (substitute var new u)
-- substitute var new (With t u)   = With (substitute var new t) (substitute var new u)
-- substitute var new (Acc t)      = Acc (substitute var new t)
-- substitute var new (Req t)      = Req (substitute var new t)
-- substitute var new (Exists t u) = Exists t (substitute var new u)
-- substitute var new (Forall t u) = Forall t (substitute var new u)
-- substitute _   _   others       = others
