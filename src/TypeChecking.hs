module TypeChecking where

-- import Syntax.Concrete
import qualified Syntax.Concrete as C
import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
import qualified Syntax.Abstract as A
import Syntax.Abstract (Session, Type(..), TypeVar(..))
import Syntax.Base
--
import Prelude hiding (lookup)

import Data.Loc (Loc)

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Debug.Trace

--------------------------------------------------------------------------------
-- | State

type Name = TermName Loc
type Chan = Name
type Term = Process Loc

data Definition = Annotated   Term (C.Session Loc)
                | Unannotated Term
                deriving (Show)

isAnnotated :: Definition -> Bool
isAnnotated (Annotated _ _) = True
isAnnotated _               = False

type CtxVar = Int

data TCState = TCState
  { stTypeCount   :: Int              -- for type variables
  , stDefinitions :: Map Name Definition
  } deriving (Show)

initialTCState :: TCState
initialTCState = TCState 0 Map.empty


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
  | DefnNotFound Term Name
  deriving (Show)

data TypeError
  = TypeSigDuplicated Name Name
  | TermDefnDuplicated Name Name
  -- | TypeSigNotFound Name
  | InferError InferError
  | Others Text
  deriving (Show)

--------------------------------------------------------------------------------
-- | Monad

type TCM = ExceptT TypeError (State TCState)

freshType :: TCM TypeVar
freshType = do
  i <- gets stTypeCount
  modify $ \ st -> st { stTypeCount = i + 1 }
  return $ Nameless i

putDefiniotions :: Program Loc -> TCM ()
putDefiniotions (Program declarations _) =
  modify $ \ st -> st { stDefinitions = Map.union termsWithTypes termsWithoutTypes }
  where
    toTypeSigPair (TypeSig n t _) = Just (n, t)
    toTypeSigPair _               = Nothing

    toTermDefnPair (TermDefn n t _) = Just (n, t)
    toTermDefnPair _                = Nothing

    typeSigs  = Map.fromList $ mapMaybe toTypeSigPair declarations
    termDefns = Map.fromList $ mapMaybe toTermDefnPair declarations

    termsWithTypes :: Map Name Definition
    termsWithTypes = fmap (uncurry Annotated) $ Map.intersectionWith (,) termDefns typeSigs

    termsWithoutTypes :: Map Name Definition
    termsWithoutTypes =  fmap Unannotated $ Map.difference termDefns typeSigs

--------------------------------------------------------------------------------
-- |

type InferM = ReaderT Term (StateT Session TCM)

freshType' :: InferM Type
freshType' = lift (lift freshType) >>= return . Var

execInferM :: Term -> Session -> InferM a -> TCM Session
execInferM term session program = execStateT (runReaderT program term) session

runInferM :: Term -> Session -> InferM a -> TCM (a, Session)
runInferM term session program = runStateT (runReaderT program term) session

evalInferM :: Term -> Session -> InferM a -> TCM a
evalInferM term session program = evalStateT (runReaderT program term) session

takeOut :: Chan -> InferM Type
takeOut chan = do
  session <- get
  (t, session') <- lift $ lift $ extractChannel chan session
  put session'
  return t

sessionShouldBeEmpty :: InferM ()
sessionShouldBeEmpty = do
  term <- ask
  session <- get
  unless (Map.null session) $
    lift $ lift $ throwError $ InferError $ ChannelNotComsumed term session


sessionShouldBeDisjoint :: Session -> Session -> InferM ()
sessionShouldBeDisjoint a b = do
  term <- ask
  let intersection = Map.intersection a b
  unless (Map.null intersection) $
    lift $ lift $ throwError $ InferError $ SessionShouldBeDisjoint term intersection

unifyOpposite :: Type -> Type -> InferM Type
unifyOpposite a b = do
  term <- ask
  (t, session') <- get >>= lift . lift . unifyOppositeAndSubstitute term a b
  put session'
  return t


-- unify the two given types, and update the give session.
-- for better error message, make the former type be the expecting type
-- and the latter be the given type
unifyAndSubstitute' :: Type -> Type -> InferM Type
unifyAndSubstitute' expected given = do
  term <- ask
  (t, session') <- get >>= lift . lift . unifyAndSubstitute term expected given
  put session'
  return t


putIn :: Chan -> Type -> InferM ()
putIn chan t = do
  term <- ask
  session <- get

  -- check if "chan" is already in the session
  when (Map.member (toAbstract chan) session) $
    lift $ lift $ throwError $ InferError $ CannotAppearInside term chan
  modify (Map.insert (toAbstract chan) t)

exclude :: Session -> InferM ()
exclude s = modify (flip Map.difference s)

sandbox :: InferM a -> InferM (a, Session)
sandbox program = do
  session <- get
  result <- program
  session' <- get
  -- restore
  put session
  return (result, session')

inferWithCurrentSession :: Term -> InferM ()
inferWithCurrentSession term = local (const term) infer

checkAll :: Program Loc -> TCM (Map (TermName Loc) A.Session)
checkAll program = do
  -- checking the definitions
  checkDuplications program
  -- store the definitions
  putDefiniotions program

  -- return the inferred sessions of unannotated definitions
  definitions <- gets stDefinitions
  Map.traverseMaybeWithKey typeCheckOrInfer definitions

typeCheckOrInfer :: Name -> Definition -> TCM (Maybe A.Session)
typeCheckOrInfer name (Annotated   term session) = do
  _ <- typeCheck name (toAbstract session) term
  return Nothing
typeCheckOrInfer _ (Unannotated term) =
  inferTerm term >>= return . Just

-- there should be only at most one type signature or term definition
checkDuplications :: Program Loc -> TCM ()
checkDuplications (Program declarations _) = do
  let typeSigNames = mapMaybe typeSigName declarations
  let termDefnNames = mapMaybe termDefnName declarations

  case getDuplicatedPair typeSigNames of
    Nothing     -> return ()
    Just (a, b) -> throwError $ TypeSigDuplicated a b
  case getDuplicatedPair termDefnNames of
    Nothing     -> return ()
    Just (a, b) -> throwError $ TermDefnDuplicated a b

  where
    getDuplicatedPair :: [TermName Loc] -> Maybe (TermName Loc, TermName Loc)
    getDuplicatedPair names =
      let dup = filter ((> 1) . length) $ List.group $ List.sort names
      in if null dup then Nothing else Just (head dup !! 0, head dup !! 1)


checkNotInSession :: Chan -> InferM ()
checkNotInSession chan = do
  term <- ask
  session <- get
  when (Map.member (toAbstract chan) session) $
    lift $ lift $ throwError $ InferError $ CannotAppearInside term chan

checkSessionShouldBeTheSame :: Session -> Session -> InferM ()
checkSessionShouldBeTheSame a b = do
  term <- ask
  unless (a == b) $
    lift $ lift $ throwError $ InferError $ SessionShouldBeTheSame term $
      Map.union
        (Map.difference a b)
        (Map.difference b a)

-- freeVariable :: Process Loc -> Set (TermName Loc)
-- freeVariable (Var x _) = Set.fromList [x, y]
-- freeVariable (Link x y _) = Set.fromList [x, y]
-- freeVariable (Compose x _ p q _) = Set.delete x $ Set.union (freeVariable p) (freeVariable q)
-- freeVariable (Output x y p q _) = Set.insert x $ Set.delete y $ Set.union (freeVariable p) (freeVariable q)
-- freeVariable (Input x y p _) = Set.insert x $ Set.delete y (freeVariable p)
-- freeVariable (SelectL x p _) = Set.insert x $ freeVariable p
-- freeVariable (SelectR x p _) = Set.insert x $ freeVariable p
-- freeVariable (Choice x p q _) = Set.insert x $ Set.union (freeVariable p) (freeVariable q)
-- freeVariable (Accept x y p _) = Set.insert x $ Set.delete y (freeVariable p)
-- freeVariable (Request x y p _) = Set.insert x $ Set.delete y (freeVariable p)
-- freeVariable (OutputT x _ p _) = Set.insert x (freeVariable p)
-- freeVariable (InputT x _ p _) = Set.insert x (freeVariable p)
-- freeVariable (EmptyOutput x _) = Set.singleton x
-- freeVariable (EmptyInput x p _) = Set.insert x $ freeVariable p
-- freeVariable (EmptyChoice x _) = Set.singleton x


inferTerm :: Term -> TCM Session
inferTerm term = infer' term Map.empty

typeCheck :: Name -> Session -> Term -> TCM ()
typeCheck name annotated term = do
  inferred <- inferTerm term
  let notInferred = Map.difference annotated inferred
  let notAnnotated = Map.difference inferred annotated
  let difference = Map.union notInferred notAnnotated

  -- see if the keys of two Maps look the same
  unless (Map.null difference) $
    throwError $ InferError $ SessionMismatch name notInferred notAnnotated

  -- look into the types and see if they are also the same
  forM_ (Map.intersectionWith (,) annotated inferred) (uncurry (checkIfEqual term))

infer' :: Term -> Session -> TCM Session
infer' term session = execInferM term session infer


infer :: InferM ()
infer = do
  term <- ask
  case term of

    Call x _ -> do
      definition <- lift $ lift $ gets stDefinitions

      case Map.lookup x definition of
        Nothing -> lift $ lift $ throwError $ InferError $ DefnNotFound term x
        Just (Annotated _ t) -> put (toAbstract t)
        Just (Unannotated p) -> inferWithCurrentSession p

      return ()

    Link x y _ -> do
        a <- takeOut x
        b <- takeOut y

        sessionShouldBeEmpty

        t <- unifyOpposite a b

        putIn x t
        putIn y (dual t)

    Compose x annotation p q _ -> do

      -- get fresh type if it's not annotated
      t <- case annotation of
            Nothing -> freshType'
            Just t  -> return (toAbstract t)


      (a, sessionP) <- sandbox $ do
        putIn x t
        inferWithCurrentSession p
        takeOut x

      exclude sessionP

      (b, sessionQ) <- sandbox $ do
        putIn x (dual t)
        inferWithCurrentSession q
        takeOut x

      sessionShouldBeDisjoint sessionP sessionQ

      put (Map.union sessionP sessionQ)

      _ <- unifyOpposite a b

      return ()

    Output x y p q _ -> do

      (a, sessionP) <- sandbox $ do
        inferWithCurrentSession p
        takeOut y

      exclude sessionP

      (b, sessionQ) <- sandbox $ do
        inferWithCurrentSession q
        takeOut x

      sessionShouldBeDisjoint sessionP sessionQ

      put (Map.union sessionP sessionQ)

      putIn x (Times a b)


    Input x y p _ -> do

      inferWithCurrentSession p
      b <- takeOut x
      a <- takeOut y

      putIn x (Par a b)

    SelectL x p _ -> do

      inferWithCurrentSession p
      a <- takeOut x
      b <- freshType'

      putIn x (Plus a b)

    SelectR x p _ -> do

      inferWithCurrentSession p
      b <- takeOut x
      a <- freshType'

      putIn x (Plus a b)

    Choice x p q _ -> do

      (a, sessionP) <- sandbox $ do
        inferWithCurrentSession p
        takeOut x

      (b, sessionQ) <- sandbox $ do
        inferWithCurrentSession q
        takeOut x

      checkSessionShouldBeTheSame sessionP sessionQ

      putIn x (With a b)

    Accept x y p _ -> do

      inferWithCurrentSession p
      a <- takeOut y

      -- weaken anything in the session that has not been weakened
      modify weaken

      checkNotInSession x

      putIn x (Acc a)

    Request x y p _ -> do

      inferWithCurrentSession p
      a <- takeOut y

      checkNotInSession x

      putIn x (Req a)

    OutputT x t p _ -> do

      inferWithCurrentSession p
      u <- takeOut x
      v <- freshType'

      putIn x (Exists Unknown v (Just (toAbstract t, u)))


    InputT x t p _ -> do

      inferWithCurrentSession p
      u <- takeOut x

      putIn x (Forall (toAbstract t) u)

    EmptyOutput x _ -> do

      _ <- takeOut x
      sessionShouldBeEmpty

      putIn x One

    EmptyInput x p _ -> do

      t <- takeOut x
      t' <- unifyAndSubstitute' Bot t

      inferWithCurrentSession p

      checkNotInSession x

      putIn x t'

    EmptyChoice x _ -> do

      t <- takeOut x
      t' <- unifyAndSubstitute' Top t

      putIn x t'

    End _ -> do

      sessionShouldBeEmpty

    Mix p q _ -> do

      (_, sessionP) <- sandbox $
        inferWithCurrentSession p

      exclude sessionP

      (_, sessionQ) <- sandbox $
        inferWithCurrentSession q


      sessionShouldBeDisjoint sessionP sessionQ

      put $ Map.union sessionP sessionQ

-- weaken everything that has not been weakened
weaken :: Session -> Session
weaken = Map.map (\t -> if weakened t then t else Req t)
  where
    weakened :: Type -> Bool
    weakened (Req _) = True
    weakened _       = False

extractChannel :: Chan -> Session -> TCM (Type, Session)
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
unifyOppositeAndSubstitute :: Term -> Type -> Type -> Session -> TCM (Type, Session)
unifyOppositeAndSubstitute term a@(Exists _ _ _) b@(Forall _ _) = unifyAndSubstitute term a (dual b)
unifyOppositeAndSubstitute term a@(Forall _ _) b@(Exists _ _ _) = unifyAndSubstitute term (dual a) b
unifyOppositeAndSubstitute term a b                             = unifyAndSubstitute term a (dual b)

-- unify the two given types, and update the give session.
-- for better error message, make the former type be the expecting type
-- and the latter be the given type
unifyAndSubstitute :: Term -> Type -> Type -> Session -> TCM (Type, Session)
unifyAndSubstitute term expected given session = do
    let (result, subst) = unify expected given
    case result of
      Left (t, u) -> throwError $ InferError $ TypeMismatch term expected given t u
      Right t -> do
        let session' = execSubstituton session subst
        return (t, session')

    where
      execSubstituton :: Session -> [Substitution] -> Session
      execSubstituton = foldr $ \ (Substitute var t) -> Map.map (substitute var t)

checkIfEqual :: Term -> Type -> Type -> TCM ()
checkIfEqual term expected given = do
    let (result, _) = unify expected given
    case result of
      Left (a, b) -> throwError $ InferError $ TypeMismatch term expected given a b
      Right _ -> return ()

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
    run (Dual     t  )  v               = dual   <$> run t        (dual v)
    run t               (Dual     v  )  = dual   <$> run (dual t) v
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
