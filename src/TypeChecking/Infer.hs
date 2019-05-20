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
      case Map.lookup x definition of
        Nothing -> throwError $ InferError $ DefnNotFound term x
        Just (Annotated _ t) -> return (toAbstract t)
        Just (Unannotated p) -> inferWith p input

    Link x y _ -> do
      a <- extract x
      b <- extract y
      t <- unifyOpposite a b
      return Map.empty

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

    Output x y p q _ -> do

      -- infer P
      a <- freshType
      sessionP <- inferWith p $ pairs [(y, a)]

      -- infer Q
      b <- freshType
      sessionQ <- inferWith q $ pairs [(x, b)]

      c <- extract x
      t <- unify c (Times a b)

      sessionShouldBeDisjoint term sessionP sessionQ

      return (Map.union sessionP sessionQ)

    Input x y p _ -> do

      a <- freshType  -- y : a
      b <- freshType  -- x : b
      session <- inferWith p $ pairs [(y, a), (x, b)]

      c <- extract x
      t <- unify c (Par a b)

      -- traceShow session (return ())
      return session

    SelectL x p _ -> do

      -- infer P
      a <- freshType
      b <- freshType
      session <- inferWith p $ pairs [(x, a)]

      c <- extract x
      t <- unify c (Plus a b)

      return session

    SelectR x p _ -> do

      -- infer P
      b <- freshType
      a <- freshType
      session <- inferWith p $ pairs [(x, b)]

      c <- extract x
      t <- unify c (Plus a b)

      return session

    Choice x p q _ -> do

      a <- freshType
      sessionP <- inferWith p $ pairs [(x, a)]

      b <- freshType
      sessionQ <- inferWith q $ pairs [(x, b)]

      c <- extract x
      t <- unify c (With a b)

      sessionShouldBeTheSame term sessionP sessionQ

      return sessionP

    Accept x y p _ -> do

      a <- freshType
      session <- inferWith p $ pairs [(y, a)]

      sessionShouldAllBeRequesting term session

      a' <- extract x
      t <- unify a' (Acc a)

      return session

    Request x y p _ -> do

      a <- freshType
      session <- inferWith p $ pairs [(y, a)]

      a' <- extract x
      t <- unify a' (Req a)

      return session

    OutputT x outputType p _ -> do


      afterSubstitution <- freshType    -- B {A / X}
      beforeSubstitution <- freshType   -- B
      session <- inferWith p $ pairs [(x, afterSubstitution)]

      body <- extract x
      body' <- unify
        body
        (Exists
                  Unknown             -- the type variable to be substituted
                  beforeSubstitution  -- the type before substitution
                  (Just
                    ( toAbstract outputType   -- the type to be substituted with
                    , afterSubstitution       -- the resulting type after substitution
                    )))

      return session


    InputT x var p _ -> do

      b <- freshType
      session <- inferWith p $ pairs [(x, b)]

      t <- extract x
      t' <- unify t (Forall (toAbstract var) b)

      return session


    EmptyOutput x _ -> do

      t <- extract x
      t' <- unify One t

      return Map.empty

    EmptyInput x p _ -> do

      sessionP <- inferWith p Map.empty

      t <- extract x
      t' <- unify Bot t

      return sessionP

    EmptyChoice x _ -> do

      t <- extract x
      t' <- unify Top t

      return Map.empty

    End _ -> return Map.empty


    Mix p q _ -> do

      sessionP <- inferWith p Map.empty
      sessionQ <- inferWith q Map.empty
      return $ Map.union sessionP sessionQ


  -- free variables may be bound by variables from `input`
  freeVars <- get

  let boundVars = Map.mapMaybe id $ Map.intersectionWith bind input freeVars
  -- remove bound vars from free vars
  modify (\ freeVars -> Map.difference freeVars boundVars)
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
    bind x          t = Nothing

    -- from the input session
    extract :: Chan -> InferM Type
    extract chan = do
      freeVars <- get
      case Map.lookup (toAbstract chan) input of
        Nothing -> do
          t <- freshTypeVar
          -- emitting free variable
          modify (Map.insert (toAbstract chan) t)
          return $ Var t
        Just t  -> return t

    -- taking extra care when unifying two opposite types
    -- because we might will lose something when taking the dual of (Exists _ _ _)
    unifyOpposite :: Type -> Type -> InferM Type
    unifyOpposite a@(Exists _ _ _) b@(Forall _ _) = unify a (dual b)
    unifyOpposite a@(Forall _ _) b@(Exists _ _ _) = unify (dual a) b
    unifyOpposite a b                             = unify a (dual b)

    -- unify the two given types, and update the give session.
    -- for better error message, make the former type be the expecting type
    -- and the latter be the given type
    unify :: Type -> Type -> InferM Type
    unify expected given = do
      let (result, subst) = U.unify expected given
      case result of
        Left (t, u) -> throwError $ InferError $ TypeMismatch term expected given t u
        Right t -> do
          tell subst
          return t



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
