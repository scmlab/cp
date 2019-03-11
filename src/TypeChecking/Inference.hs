module TypeChecking.Inference where

import qualified Syntax.Concrete as C
import Syntax.Concrete (toAbstract)
import Syntax.Abstract (Session, Type(..), TypeVar(..))
import Syntax.Base

import Prelude hiding (lookup)
-- import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.List as List
import Data.Loc (Loc)
import qualified Data.Map as Map
import Data.Text (Text)
-- import Debug.Trace

--------------------------------------------------------------------------------
-- | State

type Name = C.TermName Loc
type Chan = Name
type Term = C.Process Loc

type CtxVar = Int

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
  | SessionMismatch Name Session Session
  | SessionShouldBeAllRequesting Term Session
  | CannotAppearInside Term Chan
  | SessionShouldBeTheSame Term Session
  | SessionShouldBeDisjoint Term Session
  | ChannelNotComsumed Term Session
  deriving (Show)

--------------------------------------------------------------------------------
-- | Monad

type InferM = ExceptT InferError (State InferState)

--------------------------------------------------------------------------------
-- |
--

runInferM :: InferM a -> Either InferError a
runInferM program = evalState (runExceptT program) initState
  where
    initState = InferState 0

inferTerm :: Term -> Either InferError Session
inferTerm term = runInferM (infer term Map.empty)

typeCheck :: Name -> Session -> Term -> Either InferError ()
typeCheck name annotated term = runInferM $ do
  inferred <- infer term Map.empty
  let notInferred = Map.difference annotated inferred
  let notAnnotated = Map.difference inferred annotated
  let difference = Map.union notInferred notAnnotated

  -- see if the keys of two Maps look the same
  unless (Map.null difference) $
    throwError $ SessionMismatch name notInferred notAnnotated

  -- look into the types and see if they are also the same
  forM_ (Map.intersectionWith (,) annotated inferred) (uncurry (checkIfEqual term))


infer :: Term -> Session -> InferM Session
infer term session = case term of
  C.Link x y _ -> do

    (a, session') <- extractChannel x session
    (b, session'') <- extractChannel y session'

    unless (Map.null session'') $
      throwError $ ChannelNotComsumed term session''


    (t, session''') <- unifyOppositeAndSubstitute term a b session''

    return
      $ Map.insert (toAbstract x) t
      $ Map.insert (toAbstract y) (dual t)
      $ session'''

  C.Compose x Nothing p q _ -> do

    (t, sessionP) <- infer p session >>= extractChannel x

    -- splitting the context
    let session' = Map.difference session sessionP
    (u, sessionQ) <- infer q session' >>= extractChannel x

    checkSessionShouldBeDisjoint term sessionP sessionQ

    let session'' = Map.union sessionP sessionQ
    (_, session''') <- unifyOppositeAndSubstitute term t u session''

    return session'''

  C.Compose x (Just t) p q _ -> do

    (t', sessionP) <- infer p session >>= extractChannel x

    -- splitting the context
    let session' = Map.difference session sessionP
    (u, sessionQ) <- infer q session' >>= extractChannel x

    checkSessionShouldBeDisjoint term sessionP sessionQ

    let session'' = Map.union sessionP sessionQ
    (v, session''') <- unifyOppositeAndSubstitute term t' u session''

    -- see if the inferred type is the same as the annotated type
    checkIfEqual term (toAbstract t) v

    return session'''

  C.Output x y p q _ -> do

    (a, sessionP) <- infer p session >>= extractChannel y

    let session' = Map.difference session sessionP
    (b, sessionQ) <- infer q session' >>= extractChannel x

    checkSessionShouldBeDisjoint term sessionP sessionQ

    let session'' = Map.union sessionP sessionQ
    let t = Times a b
    return
      $ Map.insert (toAbstract x) t
      $ session''

  C.Input x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y
    (b, session'') <- extractChannel x session'

    let t = Par a b
    return
      $ Map.insert (toAbstract x) t
      $ session''

  C.SelectL x p _ -> do
    (a, session') <- infer p session >>= extractChannel x
    b <- freshType
    let t = Plus a (Var b)
    return
      $ Map.insert (toAbstract x) t
      $ session'

  C.SelectR x p _ -> do
    (b, session') <- infer p session >>= extractChannel x
    a <- freshType
    let t = Plus (Var a) b
    return
      $ Map.insert (toAbstract x) t
      $ session'

  C.Choice x p q _ -> do
    (a, sessionP) <- infer p session >>= extractChannel x
    (b, sessionQ) <- infer q session >>= extractChannel x
    checkSessionShouldBeTheSame term sessionP sessionQ
    let t = With a b
    return
      $ Map.insert (toAbstract x) t
      $ sessionP

  C.Accept x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y
    checkSessionWhenAccept term session'

    checkCannotAppearInside term x session'

    return
      $ Map.insert (toAbstract x) (Acc a)
      $ session'

  C.Request x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y

    checkCannotAppearInside term x session'

    return
      $ Map.insert (toAbstract x) (Req a)
      $ session'

  C.OutputT x t p _ -> do

    (u, session') <- infer p session >>= extractChannel x
    v <- freshType
    return
      $ Map.insert (toAbstract x) (Exists Unknown (Var v) (Just (toAbstract t, u)))
      $ session'

  C.InputT x t p _ -> do

    (u, session') <- infer p session >>= extractChannel x
    return
      $ Map.insert (toAbstract x) (Forall (toAbstract t) u)
      $ session'

  C.EmptyOutput x _ -> do

    (_, leftover) <- extractChannel x session
    unless (Map.null leftover) $
      throwError $ ChannelNotComsumed term leftover

    return
      $ Map.fromList [(toAbstract x, One)]

  C.EmptyInput x p _ -> do

    (t, session') <- extractChannel x session
    (t', session'') <- unifyAndSubstitute term Bot t session'

    session''' <- infer p session''

    checkCannotAppearInside term x session'''

    return
      $ Map.insert (toAbstract x) t'
      $ session'''

  C.EmptyChoice x _ -> do
    (t, session') <- extractChannel x session
    (t', session'') <- unifyAndSubstitute term Top t session'

    return
      $ Map.insert (toAbstract x) t'
      $ session''


extractChannel :: Chan -> Session -> InferM (Type, Session)
extractChannel chan session = do
  case Map.lookup (toAbstract chan) session of
    Nothing -> do
      t <- freshType
      return (Var t, session)
    Just t -> do
      let session' = Map.delete (toAbstract chan) session
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
    let (result, _) = unify expected given
    case result of
      Left (a, b) -> throwError $ TypeMismatch term expected given a b
      Right _ -> return ()


-- all channels should be requesting something
checkSessionWhenAccept :: Term -> Session -> InferM ()
checkSessionWhenAccept term session = do
  let result = List.all requesting $ Map.toList session
  unless result $
    throwError $ SessionShouldBeAllRequesting term session

  where
    requesting :: (Text, Type) -> Bool
    requesting (_, Req _) = True
    requesting (_,     _) = False

checkSessionShouldBeTheSame :: Term -> Session -> Session -> InferM ()
checkSessionShouldBeTheSame term a b = do
  unless (a == b) $
    throwError $ SessionShouldBeTheSame term $
      Map.union
        (Map.difference a b)
        (Map.difference b a)

checkSessionShouldBeDisjoint :: Term -> Session -> Session -> InferM ()
checkSessionShouldBeDisjoint term a b = do
  let intersection = Map.intersection a b
  unless (Map.null intersection) $
    throwError $ SessionShouldBeDisjoint term intersection

checkCannotAppearInside :: Term -> Chan -> Session -> InferM ()
checkCannotAppearInside term chan session = do
  when (Map.member (toAbstract chan) session) $
    throwError $ CannotAppearInside term chan

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
