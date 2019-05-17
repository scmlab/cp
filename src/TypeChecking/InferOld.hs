module TypeChecking.InferOld where

-- import Syntax.Concrete
import qualified Syntax.Concrete as C
import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
import qualified Syntax.Abstract as A
import Syntax.Abstract (Session, Type(..), TypeVar(..))
import Syntax.Base
import qualified TypeChecking.Unification as U
import TypeChecking.Unification (Substitution(..))
import TypeChecking.Types
--
import Prelude hiding (lookup)

import Data.Loc (Loc)

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except


--------------------------------------------------------------------------------
-- | TCM

execInferM :: Term -> Session -> InferM a -> TCM Session
execInferM term session program = execStateT (runReaderT program term) session

runInferM :: Term -> Session -> InferM a -> TCM (a, Session)
runInferM term session program = runStateT (runReaderT program term) session

evalInferM :: Term -> Session -> InferM a -> TCM a
evalInferM term session program = evalStateT (runReaderT program term) session

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
-- | InferM

type InferM = ReaderT Term (StateT Session TCM)

inferError :: InferError -> InferM a
inferError = lift . lift . throwError . InferError

freshType :: InferM Type
freshType = do
    i <- lift $ lift $ gets stTypeCount
    lift $ lift $ modify $ \ st -> st { stTypeCount = i + 1 }
    return $ Var $ Nameless i

takeOut :: Chan -> InferM Type
takeOut chan = do
  session <- get
  case Map.lookup (toAbstract chan) session of
      Nothing -> freshType
      Just t -> do
        modify (Map.delete (toAbstract chan))
        return t

sessionShouldBeEmpty :: InferM ()
sessionShouldBeEmpty = do
  term <- ask
  session <- get
  unless (Map.null session) $
    inferError $ ChannelNotComsumed term session

sessionShouldBeDisjoint :: Session -> Session -> InferM ()
sessionShouldBeDisjoint a b = do
  term <- ask
  let intersection = Map.intersection a b
  unless (Map.null intersection) $
    inferError $ SessionShouldBeDisjoint term intersection

-- taking extra care when unifying two opposite types
-- because we might will lose something when taking the dual of (Exists _ _ _)
unifyOppositeAndSubstitute :: Type -> Type -> InferM Type
unifyOppositeAndSubstitute a@(Exists _ _ _) b@(Forall _ _) = unifyAndSubstitute a (dual b)
unifyOppositeAndSubstitute a@(Forall _ _) b@(Exists _ _ _) = unifyAndSubstitute (dual a) b
unifyOppositeAndSubstitute a b                             = unifyAndSubstitute a (dual b)

-- unify the two given types, and update the give session.
-- for better error message, make the former type be the expecting type
-- and the latter be the given type
unifyAndSubstitute :: Type -> Type -> InferM Type
unifyAndSubstitute expected given = do
  term <- ask
  session <- get
  let (result, subst) = U.unify expected given
  case result of
    Left (t, u) -> inferError $ TypeMismatch term expected given t u
    Right t -> do
      modify (substitute subst)
      return t

putIn :: Chan -> Type -> InferM ()
putIn chan t = do
  term <- ask
  session <- get

  -- check if "chan" is already in the session
  when (Map.member (toAbstract chan) session) $
    inferError $ CannotAppearInside term chan
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

checkNotInSession :: Chan -> InferM ()
checkNotInSession chan = do
  term <- ask
  session <- get
  when (Map.member (toAbstract chan) session) $
    inferError $ CannotAppearInside term chan

checkSessionShouldBeTheSame :: Session -> Session -> InferM ()
checkSessionShouldBeTheSame a b = do
  term <- ask
  unless (a == b) $
    inferError $ SessionShouldBeTheSame term $
      Map.union
        (Map.difference a b)
        (Map.difference b a)

inferTerm :: Term -> TCM Session
inferTerm term = execInferM term Map.empty infer

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

infer :: InferM ()
infer = do
  term <- ask
  case term of

    Call x _ -> do
      definition <- lift $ lift $ gets stDefinitions

      case Map.lookup x definition of
        Nothing -> inferError $ DefnNotFound term x
        Just (Annotated _ t) -> put (toAbstract t)
        Just (Unannotated p) -> inferWithCurrentSession p

      return ()

    -- w : A
    -- x : A'
    -- A = ¬ A'
    -----------------------------
    -- x ↔ y ⊢ w : A  , x : A'

    -- input  : x ↔ y, w, x

    --  w  A


    -- assume known   : x, y
    -- show   known   : w
    -- show unknown   : A

    -- assume known   : A
    -- show   known   : x
    -- show unknown   : A'

    -- assume known   : A'
    -- show   known   : A = ¬ A'
    -- show unknown   : result of A = ¬ A'

    -- assume known   : result of A = ¬ A'
    -- show   known   : w : A  , x : A'

    -- output : w : A  , x : A'

    Link x y _ -> do
        a <- takeOut x
        b <- takeOut y

        sessionShouldBeEmpty

        t <- unifyOppositeAndSubstitute a b

        putIn x t
        putIn y (dual t)

    -- x : A
    -- P ⊢ Γ, x :   A
    -- Q ⊢ Δ, x : ¬ A
    ----------------------
    -- ν x : (P | Q) ⊢ Γ , Δ

    -- input  : ν x : (P | Q)

    -- assume known : x, P, Q
    -- show   known : x
    -- show unknown : A

    -- assume known : P
    -- show   known : P
    -- show unknown : P ⊢ Γ, x : A

    -- assume known : Γ
    -- show   known : P

    Compose x annotation p q _ -> do

      -- get fresh type if it's not annotated
      t <- case annotation of
            Nothing -> freshType
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

      _ <- unifyOppositeAndSubstitute a b

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
      b <- freshType

      putIn x (Plus a b)

    SelectR x p _ -> do

      inferWithCurrentSession p
      b <- takeOut x
      a <- freshType

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
      v <- freshType

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
      t' <- unifyAndSubstitute Bot t

      inferWithCurrentSession p

      checkNotInSession x

      putIn x t'

    EmptyChoice x _ -> do

      t <- takeOut x
      t' <- unifyAndSubstitute Top t

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


checkIfEqual :: Term -> Type -> Type -> TCM ()
checkIfEqual term expected given = do
    let (result, _) = U.unify expected given
    case result of
      Left (a, b) -> throwError $ InferError $ TypeMismatch term expected given a b
      Right _ -> return ()


substitute :: [Substitution] -> Session -> Session
substitute = flip $ foldr $ \ (Substitute var t) -> Map.map (U.substitute var t)
