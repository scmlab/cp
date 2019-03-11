module TypeChecking.Inference where

import qualified Syntax.Concrete as C
import Syntax.Abstract (Type(..), TypeVar(..))
import Syntax.Base

import Prelude hiding (lookup)
-- import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.List as List
import Data.Loc (Loc)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

-- import Debug.Trace

--------------------------------------------------------------------------------
-- | State

type Chan = C.TermName Loc
type Term = C.Process Loc

type CtxVar = Int

type Session = Map Chan Type

data InferState = InferState
  {
    stTypeCount :: Int              -- for type variables
  } deriving (Show)

freshType :: InferM TypeVar
freshType = do
  i <- gets stTypeCount
  modify $ \ st -> st { stTypeCount = i + 1 }
  return $ Nameless i

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  | TypeMismatch Term Type Type Type Type
  | ContextShouldBeAllRequesting Term Session
  | CannotAppearInside Term Chan
  | ContextShouldBeTheSame Term Session
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
    initState = InferState 0

infer :: Term -> Session -> InferM Session
infer term session = case term of
  C.Link x y _ -> do

    (a, session') <- extractChannel x session
    (b, session'') <- extractChannel y session'

    unless (Map.null session'') $
      throwError $ ChannelNotComsumed term session''


    (t, session''') <- unifyOppositeAndSubstitute term a b session''

    return
      $ Map.insert x t
      $ Map.insert y (dual t)
      $ session'''

  C.Compose x Nothing p q _ -> do

    (t, sessionP) <- infer p session >>= extractChannel x

    -- splitting the context
    let session' = Map.difference session sessionP
    (u, sessionQ) <- infer q session' >>= extractChannel x


    let session'' = Map.union sessionP sessionQ
    (_, session''') <- unifyOppositeAndSubstitute term t u session''

    return session'''

  C.Compose x (Just t) p q _ -> do

    (t', sessionP) <- infer p session >>= extractChannel x

    checkIfEqual term (C.toAbstract t) t'

    -- splitting the context
    let session' = Map.difference session sessionP
    (u, sessionQ) <- infer q session' >>= extractChannel x


    let session'' = Map.union sessionP sessionQ
    (_, session''') <- unifyOppositeAndSubstitute term t' u session''

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

    (a, session') <- infer p session >>= extractChannel y
    (b, session'') <- extractChannel x session'

    let t = Par a b
    return
      $ Map.insert x t
      $ session''

  C.SelectL x p _ -> do
    (a, session') <- infer p session >>= extractChannel x
    b <- freshType
    let t = Plus a (Var b)
    return
      $ Map.insert x t
      $ session'

  C.SelectR x p _ -> do
    (b, session') <- infer p session >>= extractChannel x
    a <- freshType
    let t = Plus (Var a) b
    return
      $ Map.insert x t
      $ session'

  C.Choice x p q _ -> do
    (a, sessionP) <- infer p session >>= extractChannel x
    (b, sessionQ) <- infer q session >>= extractChannel x
    checkContextShouldBeTheSame term sessionP sessionQ
    let t = With a b
    return
      $ Map.insert x t
      $ sessionP

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

  C.OutputT x t p _ -> do

    (u, session') <- infer p session >>= extractChannel x
    v <- freshType
    return
      $ Map.insert x (Exists Unknown (Var v) (Just (C.toAbstract t, u)))
      $ session'

  C.InputT x t p _ -> do

    (u, session') <- infer p session >>= extractChannel x
    return
      $ Map.insert x (Forall (C.toAbstract t) u)
      $ session'

  C.EmptyOutput x _ -> do

    (_, leftover) <- extractChannel x session
    unless (Map.null leftover) $
      throwError $ ChannelNotComsumed term leftover

    return
      $ Map.fromList [(x, One)]

  C.EmptyInput x p _ -> do

    (t, session') <- extractChannel x session
    (t', session'') <- unifyAndSubstitute term Bot t session'

    session''' <- infer p session''

    if Map.member x session'''
      then throwError $ CannotAppearInside p x
      else return
            $ Map.insert x t'
            $ session'''

  C.EmptyChoice x _ -> do
    (t, session') <- extractChannel x session
    (t', session'') <- unifyAndSubstitute term Top t session'

    return
      $ Map.insert x t'
      $ session''


extractChannel :: Chan -> Session -> InferM (Type, Session)
extractChannel chan session = do
  case Map.lookup chan session of
    Nothing -> do
      t <- freshType
      return (Var t, session)
    Just t -> do
      let session' = Map.delete chan session
      return (t, session')

-- taking extra care when unifying two opposite types
-- because we might will lose something when taking the dual of (Exists _ _ _)
unifyOppositeAndSubstitute :: Term -> Type -> Type -> Session -> InferM (Type, Session)
unifyOppositeAndSubstitute term a@(Exists _ _ _) b@(Forall _ _) = unifyAndSubstitute term a (dual b)
unifyOppositeAndSubstitute term a@(Forall _ _) b@(Exists _ _ _) = unifyAndSubstitute term (dual a) b
unifyOppositeAndSubstitute term a b                             = unifyAndSubstitute term a (dual b)

-- unify the two given types, and update the give session.
-- for better error message, make the former type be the expecting type
-- and the latter be the given type
unifyAndSubstitute :: Term -> Type -> Type -> Session -> InferM (Type, Session)
unifyAndSubstitute term expected given session = do
    let (result, subst) = unify expected given
    case result of
      Left (t, u) -> throwError $ TypeMismatch term expected given t u
      Right t -> do
        let session' = execSubstituton session subst
        return (t, session')

    where
      execSubstituton :: Session -> [Substitution] -> Session
      execSubstituton = foldr $ \ (Substitute var t) -> Map.map (substitute var t)

checkIfEqual :: Term -> Type -> Type -> InferM ()
checkIfEqual term expected given = do
    let (result, subst) = unify expected given
    case result of
      Left (a, b) -> throwError $ TypeMismatch term expected given a b
      Right t -> return ()


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
    run (Var        i)  v               = do
      modify ((:) (Substitute i v))
      return v
    run t               (Var        j)  = do
      modify ((:) (Substitute j t))
      return t
    -- run (Subst  t x u)  (Subst  v y w)  = Subst  <$> run t v <*> (modify ((:) (Substitute x (Var y))) >> return y) <*> run u w
    run (Dual     t  )  v               = run t        (dual v)
    run t               (Dual     v  )  = run (dual t) v
    run (Times    t u)  (Times    v w)  = Times  <$> run t v <*> run u w
    run (Par      t u)  (Par      v w)  = Par    <$> run t v <*> run u w
    run (Plus     t u)  (Plus     v w)  = Plus   <$> run t v <*> run u w
    run (With     t u)  (With     v w)  = With   <$> run t v <*> run u w
    run (Acc      t  )  (Acc      v  )  = run t v
    run (Req      t  )  (Req      v  )  = run t v
    run (Exists   _ u Nothing) (Exists   _ w Nothing)  = run u w
    -- happens when we are composing ∃ with ∀
    run (Exists   _ (Var ghost) (Just (witness, substituted))) (Exists var body Nothing) = do
      -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run substituted (substitute var witness body)
    run (Exists var body Nothing)  (Exists   _ (Var ghost) (Just (witness, substituted))) = do
      -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run (substitute var witness body) substituted
    run (Forall   _ u)  (Forall   _ w)  = run u w
    run One             One             = return One
    run Top             Top             = return Top
    run Zero            Zero            = return Zero
    run Bot             Bot             = return Bot
    run t               v               = throwError (t, v)

-- replace a type variable in some type with another type
substitute :: TypeVar -> Type -> Type -> Type
substitute var new (Var var')
  | var ==      var' = new
  | otherwise        = Var var'
substitute var new (Dual t)       = Dual (substitute var new t)
substitute var new (Times t u)    = Times (substitute var new t) (substitute var new u)
substitute var new (Par t u)      = Par (substitute var new t) (substitute var new u)
substitute var new (Plus t u)     = Plus (substitute var new t) (substitute var new u)
substitute var new (With t u)     = With (substitute var new t) (substitute var new u)
substitute var new (Acc t)        = Acc (substitute var new t)
substitute var new (Req t)        = Req (substitute var new t)
substitute var new (Exists t u _) = Exists t (substitute var new u) Nothing
substitute var new (Forall t u)   = Forall t (substitute var new u)
substitute _   _   others         = others

-- all channels should be requesting something
checkContextWhenAccept :: Term -> Session -> InferM ()
checkContextWhenAccept term session = do
  let result = List.all requesting $ Map.toList session
  unless result $
    throwError $ ContextShouldBeAllRequesting term session

  where
    requesting :: (Chan, Type) -> Bool
    requesting (_, Req _) = True
    requesting (_,     _) = False

checkContextShouldBeTheSame :: Term -> Session -> Session -> InferM ()
checkContextShouldBeTheSame term a b = do
  unless (a == b) $
    throwError $ ContextShouldBeTheSame term $
      Map.union
        (Map.difference a b)
        (Map.difference b a)
