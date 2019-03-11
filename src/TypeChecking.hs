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
import Control.Monad.Except

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
inferTerm term = infer term Map.empty

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


infer :: Term -> Session -> TCM Session
infer term session = case term of

  Call x _ -> do
    definition <- gets stDefinitions

    case Map.lookup x definition of
      Nothing -> throwError $ InferError $ DefnNotFound term x
      Just (Annotated _ t) -> return (toAbstract t)
      Just (Unannotated p) -> infer p session


  Link x y _ -> do

    (a, session') <- extractChannel x session
    (b, session'') <- extractChannel y session'

    unless (Map.null session'') $
      throwError $ InferError $ ChannelNotComsumed term session''


    (t, session''') <- unifyOppositeAndSubstitute term a b session''

    return
      $ Map.insert (toAbstract x) t
      $ Map.insert (toAbstract y) (dual t)
      $ session'''

  Compose x Nothing p q _ -> do

    (t, sessionP) <- infer p session >>= extractChannel x

    -- splitting the context
    let session' = Map.difference session sessionP
    (u, sessionQ) <- infer q session' >>= extractChannel x

    checkSessionShouldBeDisjoint term sessionP sessionQ

    let session'' = Map.union sessionP sessionQ
    (_, session''') <- unifyOppositeAndSubstitute term t u session''

    return session'''

  Compose x (Just t) p q _ -> do

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

  Output x y p q _ -> do

    (a, sessionP) <- infer p session >>= extractChannel y

    let session' = Map.difference session sessionP
    (b, sessionQ) <- infer q session' >>= extractChannel x

    checkSessionShouldBeDisjoint term sessionP sessionQ

    let session'' = Map.union sessionP sessionQ
    let t = Times a b
    return
      $ Map.insert (toAbstract x) t
      $ session''

  Input x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y
    (b, session'') <- extractChannel x session'

    let t = Par a b
    return
      $ Map.insert (toAbstract x) t
      $ session''

  SelectL x p _ -> do
    (a, session') <- infer p session >>= extractChannel x
    b <- freshType
    let t = Plus a (Var b)
    return
      $ Map.insert (toAbstract x) t
      $ session'

  SelectR x p _ -> do
    (b, session') <- infer p session >>= extractChannel x
    a <- freshType
    let t = Plus (Var a) b
    return
      $ Map.insert (toAbstract x) t
      $ session'

  Choice x p q _ -> do
    (a, sessionP) <- infer p session >>= extractChannel x
    (b, sessionQ) <- infer q session >>= extractChannel x
    checkSessionShouldBeTheSame term sessionP sessionQ
    let t = With a b
    return
      $ Map.insert (toAbstract x) t
      $ sessionP

  Accept x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y
    checkSessionWhenAccept term session'

    checkCannotAppearInside term x session'

    return
      $ Map.insert (toAbstract x) (Acc a)
      $ session'

  Request x y p _ -> do

    (a, session') <- infer p session >>= extractChannel y

    checkCannotAppearInside term x session'

    return
      $ Map.insert (toAbstract x) (Req a)
      $ session'

  OutputT x t p _ -> do

    (u, session') <- infer p session >>= extractChannel x
    v <- freshType
    return
      $ Map.insert (toAbstract x) (Exists Unknown (Var v) (Just (toAbstract t, u)))
      $ session'

  InputT x t p _ -> do

    (u, session') <- infer p session >>= extractChannel x
    return
      $ Map.insert (toAbstract x) (Forall (toAbstract t) u)
      $ session'

  EmptyOutput x _ -> do

    (_, leftover) <- extractChannel x session
    unless (Map.null leftover) $
      throwError $ InferError $ ChannelNotComsumed term leftover

    return
      $ Map.fromList [(toAbstract x, One)]

  EmptyInput x p _ -> do

    (t, session') <- extractChannel x session
    (t', session'') <- unifyAndSubstitute term Bot t session'

    session''' <- infer p session''

    checkCannotAppearInside term x session'''

    return
      $ Map.insert (toAbstract x) t'
      $ session'''

  EmptyChoice x _ -> do
    (t, session') <- extractChannel x session
    (t', session'') <- unifyAndSubstitute term Top t session'

    return
      $ Map.insert (toAbstract x) t'
      $ session''

  End _ -> do
    unless (Map.null session) $
      throwError $ InferError $ ChannelNotComsumed term session

    return Map.empty


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


-- all channels should be requesting something
checkSessionWhenAccept :: Term -> Session -> TCM ()
checkSessionWhenAccept term session = do
  let result = List.all requesting $ Map.toList session
  unless result $
    throwError $ InferError $ SessionShouldBeAllRequesting term session

  where
    requesting :: (Text, Type) -> Bool
    requesting (_, Req _) = True
    requesting (_,     _) = False

checkSessionShouldBeTheSame :: Term -> Session -> Session -> TCM ()
checkSessionShouldBeTheSame term a b = do
  unless (a == b) $
    throwError $ InferError $ SessionShouldBeTheSame term $
      Map.union
        (Map.difference a b)
        (Map.difference b a)

checkSessionShouldBeDisjoint :: Term -> Session -> Session -> TCM ()
checkSessionShouldBeDisjoint term a b = do
  let intersection = Map.intersection a b
  unless (Map.null intersection) $
    throwError $ InferError $ SessionShouldBeDisjoint term intersection

checkCannotAppearInside :: Term -> Chan -> Session -> TCM ()
checkCannotAppearInside term chan session = do
  when (Map.member (toAbstract chan) session) $
    throwError $ InferError $ CannotAppearInside term chan

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
