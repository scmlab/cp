module TypeChecking.Infer where

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

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer hiding (Dual)
import Control.Monad.Except

import Debug.Trace

--------------------------------------------------------------------------------
-- | InferM

execInferM :: TypeVars -> InferM a -> TCM TypeVars
execInferM freeVars program = execStateT (runWriterT program) freeVars

runInferM :: TypeVars -> InferM a -> TCM ((a, [Substitution]), TypeVars)
runInferM freeVars program = runStateT (runWriterT program) freeVars

evalInferM :: TypeVars -> InferM a -> TCM (a, [Substitution])
evalInferM freeVars program = evalStateT (runWriterT program) freeVars

inferError :: InferError -> InferM a
inferError = lift . throwError . InferError

sessionShouldBeEmpty :: Term -> Session -> InferM ()
sessionShouldBeEmpty term session = do
  unless (Map.null session) $
    inferError $ ChannelNotComsumed term session

sessionShouldBeDisjoint :: Term -> Session -> Session -> InferM ()
sessionShouldBeDisjoint term a b = do
  let intersection = Map.intersection a b
  unless (Map.null intersection) $
    inferError $ SessionShouldBeDisjoint term intersection

sessionShouldBeTheSame :: Term -> Session -> Session -> InferM ()
sessionShouldBeTheSame term a b = do
  unless (a == b) $
    inferError $ SessionShouldBeTheSame term $
      Map.union
        (Map.difference a b)
        (Map.difference b a)

sessionShouldAllBeRequesting :: Term -> Session -> InferM ()
sessionShouldAllBeRequesting term session = do
  unless (Map.null outliers) $
    inferError $ SessionShouldAllBeRequesting term outliers
  where
    outliers :: Session
    outliers = Map.filter (not . isRequesting) session

    isRequesting :: Type -> Bool
    isRequesting (Req _) = True
    isRequesting _       = False

inferTerm :: Term -> TCM Session
inferTerm term = do
  ((result, substitutions), freeChannels) <- runInferM Map.empty (inferWith term Map.empty)
  return $ Map.union (substitute substitutions result) (substitute substitutions (fmap Var freeChannels))

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


--------------------------------------------------------------------------------
-- | InferM


type TypeVars = Map A.TermName TypeVar
type InferM = WriterT [Substitution] (StateT TypeVars TCM)


inferWith :: Term           -- the term to infer
          -> Session        -- channels that we know exist in the session
          -> InferM Session  -- other channels that we didn't know before
inferWith term input = do

  result <- case term of
    Call x _ -> do
      definition <- lift $ lift $ gets stDefinitions
      case Map.lookup (toAbstract x) definition of
        Nothing -> throwError $ InferError $ DefnNotFound term x
        Just (Annotated _ _ t) -> return (toAbstract t)
        Just (Unannotated _ p) -> inferWith p input

    -- {A}
    -- {B}
    -- {A = ¬ B}
    -- -----------------------------
    -- x ↔ y ⊢ {x : A , y : B}

    -- conclusion input   : assume that x and y are known

    -- premise 1  input   : vacuous
    -- premise 1  output  : show that A is unknown
    -- premise 1  output  : assume that A is known

    -- premise 2  input   : vacuous
    -- premise 2  output  : show that B is unknown
    -- premise 2  output  : assume that B is known

    -- premise 3  input   : show    that A , B are known
    -- premise 3  output  : show    that A = ¬ B is unknown
    -- premise 3  output  : assume  that A = ¬ B is known

    -- conclusion output  : show that x : A and y : B are known

    Link x y _ -> do
      a <- extract x
      b <- extract y
      unifyOpposite a b
      return Map.empty
    --
    -- {A}
    -- P ⊢ {Γ}, x : A
    --
    -- {B}
    -- Q ⊢ {Δ}, y : B
    --
    -- {A = ¬ B}
    -- -----------------------------
    -- ν x . (P | Q) ⊢ {Γ , Δ}

    -- conclusion input   : assume that x , P and Q are known

    -- premise 1  input   : vacuous
    -- premise 1  output  : show    that A is unknown
    -- premise 1  output  : assume  that A is known

    -- premise 2  input   : show    that P , x and A are known
    -- premise 2  output  : show    that Γ is unknown
    -- premise 2  output  : assume  that Γ is known

    -- premise 3  input   : vacuous
    -- premise 3  output  : show    that B is unknown
    -- premise 3  output  : assume  that B is known

    -- premise 4  input   : show    that Q , y and B are known
    -- premise 4  output  : show    that Δ is unknown
    -- premise 4  output  : assume  that Δ is known

    -- premise 5  input   : show    that A , B are known
    -- premise 5  output  : show    that A = ¬ B is unknown
    -- premise 5  output  : assume  that A = ¬ B is known

    -- conclusion output  : show that Γ and Δ are known

    Compose x annotation p q _ -> do

      -- generate fresh type for if not annotated
      t <- case annotation of
            Nothing -> freshType
            Just t  -> return (toAbstract t)

      -- infer P
      sessionP <- inferWith p $ pairs [(x, t)]

      -- infer Q
      sessionQ <- inferWith q $ pairs [(x, dual t)]


      return (Map.union sessionP sessionQ)

    -- {A}
    -- P ⊢ {Γ}, y : A
    --
    -- {B}
    -- Q ⊢ {Δ}, x : B

    -- {C}
    -- {C = A ⊗ B}

    -- {Γ and Δ be disjoint}
    -- -----------------------------
    -- x[y] . (P | Q) ⊢ {Γ , Δ , x : C}

    -- conclusion input   : assume that x , y , P , Q are known

    -- premise 1  input   : vacuous
    -- premise 1  output  : show    that A is unknown
    -- premise 1  output  : assume  that A is known

    -- premise 2  input   : show    that P , y , A are known
    -- premise 2  output  : show    that Γ is unknown
    -- premise 2  output  : assume  that Γ is known

    -- premise 3  input   : vacuous
    -- premise 3  output  : show    that B is unknown
    -- premise 3  output  : assume  that B is known

    -- premise 4  input   : show    that Q , x , B are known
    -- premise 4  output  : show    that Δ is unknown
    -- premise 4  output  : assume  that Δ is known

    -- premise 5  input   : vacuous
    -- premise 5  output  : show    that C is unknown
    -- premise 5  output  : assume  that C is known

    -- premise 6  input   : show    that C , A , B are known
    -- premise 6  output  : show    that C = A ⊗ B is unknown
    -- premise 6  output  : assume  that C = A ⊗ B is known

    -- premise 5  input   : vacuous
    -- premise 5  output  : show    that Γ and Δ 's disjointness is unknown
    -- premise 5  output  : assume  that Γ and Δ 's disjointness is known

    -- conclusion output  : show that Γ , Δ and x : C are known

    Output x y p q _ -> do

      -- infer P
      a <- freshType
      sessionP <- inferWith p $ pairs [(y, a)]

      -- infer Q
      b <- freshType
      sessionQ <- inferWith q $ pairs [(x, b)]

      c <- extract x
      unify c (Times a b)

      sessionShouldBeDisjoint term sessionP sessionQ

      return (Map.union sessionP sessionQ)

    -- {A}
    -- {B}
    -- P ⊢ {Γ}, y : A, x : B

    -- {C}
    -- {C = A ⅋ B}
    -- -----------------------------
    -- x(y) . P ⊢ {Γ , x : C}

    Input x y p _ -> do

      a <- freshType  -- y : a
      b <- freshType  -- x : b
      session <- inferWith p $ pairs [(y, a), (x, b)]

      c <- extract x
      unify c (Par a b)

      return session

    -- {A}
    -- {B}
    -- P ⊢ {Γ}, y : A

    -- {C}
    -- {C = A ⊕ B}
    -- -----------------------------
    -- x[inl] . P ⊢ {Γ , x : C}

    SelectL x p _ -> do

      -- infer P
      a <- freshType
      b <- freshType
      session <- inferWith p $ pairs [(x, a)]

      c <- extract x
      unify c (Plus a b)

      return session

    -- {A}
    -- {B}
    -- P ⊢ {Γ}, x : B

    -- {C}
    -- {C = A ⊕ B}
    -- -----------------------------
    -- x[inr] . P ⊢ {Γ , x : C}

    SelectR x p _ -> do

      -- infer P
      b <- freshType
      a <- freshType
      session <- inferWith p $ pairs [(x, b)]

      c <- extract x
      unify c (Plus a b)

      return session

    -- {A}
    -- P ⊢ {Γ}, x : A
    --
    -- {B}
    -- Q ⊢ {Δ}, x : B

    -- {C}
    -- {C = A & B}
    --
    -- {Γ = Δ}
    -- -----------------------------
    -- x.case(P, Q) ⊢ {Γ , x : C}

    -- conclusion input   : assume that x , P , Q are known

    -- premise 1  input   : vacuous
    -- premise 1  output  : show    that A is unknown
    -- premise 1  output  : assume  that A is known

    -- premise 2  input   : show    that P , x , A are known
    -- premise 2  output  : show    that Γ is unknown
    -- premise 2  output  : assume  that Γ is known

    -- premise 3  input   : vacuous
    -- premise 3  output  : show    that B is unknown
    -- premise 3  output  : assume  that B is known

    -- premise 4  input   : show    that Q , x , B are known
    -- premise 4  output  : show    that Δ is unknown
    -- premise 4  output  : assume  that Δ is known

    -- premise 5  input   : vacuous
    -- premise 5  output  : show    that C is unknown
    -- premise 5  output  : assume  that C is known

    -- premise 6  input   : show    that C , A , B are known
    -- premise 6  output  : show    that C = A & B is unknown
    -- premise 6  output  : assume  that C = A & B is known

    -- premise 7  input   : show    that Γ , Δ are known
    -- premise 7  output  : show    that Γ = Δ is unknown
    -- premise 7  output  : assume  that Γ = Δ is known

    -- conclusion output  : show that Γ and x : C are known

    Choice x p q _ -> do

      a <- freshType
      sessionP <- inferWith p $ pairs [(x, a)]

      b <- freshType
      sessionQ <- inferWith q $ pairs [(x, b)]

      c <- extract x
      unify c (With a b)

      sessionShouldBeTheSame term sessionP sessionQ

      return sessionP

    -- {A}
    -- P ⊢ {Γ}, y : A
    -- {?Γ}

    -- {B}
    -- {B = !A}
    -- -----------------------------
    -- !x(y).P ⊢ {Γ , x : B}

    Accept x y p _ -> do

      a <- freshType
      session <- inferWith p $ pairs [(y, a)]

      sessionShouldAllBeRequesting term session

      a' <- extract x
      unify a' (Acc a)

      return session

    -- {A}
    -- P ⊢ {Γ}, y : A
    --
    -- {B}
    -- {B = ?A}
    -- -----------------------------
    -- ?x[y].P ⊢ {Γ , x : B}

    Request x y p _ -> do

      a <- freshType
      session <- inferWith p $ pairs [(y, a)]

      a' <- extract x
      unify a' (Req a)

      return session

    -- {B[A/X]}
    -- P ⊢ {Γ}, x : B[A/X]

    -- {B}
    -- {C}
    -- {C = ∃_.B}
    -- -----------------------------
    -- x[A].P ⊢ {Γ , x : C}

    -- conclusion input   : assume that x , A, P are known

    -- premise 1  input   : vacuous
    -- premise 1  output  : show    that B[A/X] is unknown
    -- premise 1  output  : assume  that B[A/X] is known

    -- premise 2  input   : show    that P , x , [A/X] are known
    -- premise 2  output  : show    that Γ is unknown
    -- premise 2  output  : assume  that Γ is known

    -- premise 3  input   : vacuous
    -- premise 3  output  : show    that B is unknown
    -- premise 3  output  : assume  that B is known

    -- premise 4  input   : vacuous
    -- premise 4  output  : show    that C is unknown
    -- premise 4  output  : assume  that C is known

    -- premise 5  input   : show    that C , B , A , B[A/X] are known
    -- premise 5  output  : show    that C = ∃_.B is unknown
    -- premise 5  output  : assume  that C = ∃_.B is known

    -- conclusion output  : show that Γ and x : C are known

    OutputT x outputType p _ -> do


      afterSubstitution <- freshType    -- B {A / X}
      session <- inferWith p $ pairs [(x, afterSubstitution)]

      beforeSubstitution <- freshType   -- B
      t <- extract x
      unify
        t
        (Exists
                  Unknown                     -- the type variable to be substituted
                  beforeSubstitution          -- the type before substitution
                  (Just
                    ( toAbstract outputType   -- the type to be substituted with
                    , afterSubstitution       -- the resulting type after substitution
                    )))

      return session


    -- {B}
    -- P ⊢ {Γ}, x : B

    -- {C}
    -- {C = ∀X.B}
    -- -----------------------------
    -- x(X).P ⊢ {Γ , x : C}

    InputT x var p _ -> do

      b <- freshType
      session <- inferWith p $ pairs [(x, b)]

      t <- extract x
      unify t (Forall (toAbstract var) b)

      return session

    -- {C}
    -- {C = 1}
    -- -----------------------------
    -- x[].0 ⊢ {x : C}


    EmptyOutput x _ -> do

      t <- extract x
      unify One t

      return Map.empty

    -- P ⊢ {Γ}
    -- {C}
    -- {C = ⊥}
    -- -----------------------------
    -- x().P ⊢ {Γ , x : C}

    EmptyInput x p _ -> do

      sessionP <- inferWith p Map.empty

      t <- extract x
      unify Bot t

      return sessionP

    -- TODO: fix that spurious Map.empty
    -- {Γ}
    -- {C}
    -- {C = ⊤}
    -- -----------------------------
    -- x.case() ⊢ {Γ , x : C}

    EmptyChoice x _ -> do

      t <- extract x
      unify Top t

      return Map.empty

    -- -----------------------------
    -- end ⊢ {}

    End _ -> return Map.empty


    -- P ⊢ {Γ}
    -- Q ⊢ {Δ}
    -- -----------------------------
    -- P | Q ⊢ {Γ , Δ}

    Mix p q _ -> do

      sessionP <- inferWith p Map.empty
      sessionQ <- inferWith q Map.empty
      return $ Map.union sessionP sessionQ


  -- free variables may be bound by variables from `input`
  freeVars <- get

  let boundVars = Map.mapMaybe id $ Map.intersectionWith bind input freeVars
  -- remove bound vars from free vars
  modify (flip Map.difference boundVars)
  -- substitute free variables thrown by the sub clauses with the ones from `input`
  tell $ Map.elems boundVars

  return result

  where
    -- binding variables from `input` with free variables
    bind  :: Type -- binder
          -> TypeVar -- free variable
          -> Maybe Substitution
    bind (Var var)  t = Just $ Substitute var (Var t)
    bind (Dual var) t = bind var (dual t)
    bind _          _ = Nothing

    -- from the input session
    extract :: Chan -> InferM Type
    extract chan = do
      case Map.lookup (toAbstract chan) input of
        Nothing -> do
          t <- freshTypeVar
          -- emitting free variable
          modify (Map.insert (toAbstract chan) t)
          return $ Var t
        Just t  -> return t

    -- taking extra care when unifying two opposite types
    -- because we might will lose something when taking the dual of (Exists _ _ _)
    unifyOpposite :: Type -> Type -> InferM ()
    unifyOpposite a@(Exists _ _ _) b@(Forall _ _) = unify a (dual b)
    unifyOpposite a@(Forall _ _) b@(Exists _ _ _) = unify (dual a) b
    unifyOpposite a b                             = unify a (dual b)

    -- unify the two given types, and update the give session.
    -- for better error message, make the former type be the expecting type
    -- and the latter be the given type
    unify :: Type -> Type -> InferM ()
    unify expected given = do
      let (result, subst) = U.unify expected given
      case result of
        Left (t, u) -> throwError $ InferError $ TypeMismatch term expected given t u
        Right _ -> tell subst

    freshTypeVar :: InferM TypeVar
    freshTypeVar = do
        i <- lift $ lift $ gets stTypeCount
        lift $ lift $ modify $ \ st -> st { stTypeCount = i + 1 }
        return $ Nameless i


    freshType :: InferM Type
    freshType = Var <$> freshTypeVar

    pairs :: [(C.TermName Loc, Type)] -> Session
    pairs = Map.fromList . map (\(c, t) -> (toAbstract c, t))


  -- where
  --   execSubstituton :: Session -> [Substitution] -> Session

--