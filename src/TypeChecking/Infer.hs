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

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Debug.Trace

--------------------------------------------------------------------------------
-- | TCM

execInferM :: Session -> InferM a -> TCM Session
execInferM session program = execStateT program session

runInferM :: Session -> InferM a -> TCM (a, Session)
runInferM session program = runStateT program session

evalInferM :: Session -> InferM a -> TCM a
evalInferM session program = evalStateT program session

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
  ((subst, result), freeChannels) <- runInferM Map.empty (inferWith term Map.empty)
  -- traceShow (subst, freeChannels) (return ())
  return $ Map.union (substitute subst result) (substitute subst freeChannels)


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


type InferM = StateT Session TCM


inferWith :: Term           -- the term to infer
          -> Session        -- channels that we know exist in the session
          -> InferM  ( [Substitution] -- substitutions
                      , Session  -- other channels that we didn't know before
                      )
inferWith term input = case term of

  Call x _ -> do
    definition <- lift $ gets stDefinitions
    case Map.lookup x definition of
      Nothing -> throwError $ InferError $ DefnNotFound term x
      Just (Annotated _ t) -> return ([], toAbstract t)
      Just (Unannotated p) -> inferWith p input

  Link x y _ -> do
    a <- extract x
    b <- extract y
    (t , substitute) <- unifyOpposite a b
    return (substitute, Map.empty)

  Compose x annotation p q _ -> do

    -- generate fresh type for if not annotated
    t <- case annotation of
          Nothing -> freshType
          Just t  -> return (toAbstract t)

    -- infer P
    (substituteP, othersP) <- inferWith p $ pairs [(x, t)]

    -- infer Q
    (substituteQ, othersQ) <- inferWith q $ dualSession $ pairs [(x, t)]

    return (substituteP ++ substituteQ, Map.union othersP othersQ)

  Output x y p q _ -> do

    -- infer P
    a <- freshType
    (substituteP, sessionP) <- inferWith p $ pairs [(y, a)]

    -- infer Q
    b <- freshType
    (substituteQ, sessionQ) <- inferWith q $ pairs [(x, b)]

    c <- extract x
    (t , substitute) <- unify c (Times a b)

    sessionShouldBeDisjoint term sessionP sessionQ

    return (substituteP ++ substituteQ ++ substitute, Map.union sessionP sessionQ)

  Input x y p _ -> do

    a <- freshType  -- y : a
    b <- freshType  -- x : b
    (substituteP, sessionP) <- inferWith p $ pairs [(y, a), (x, b)]

    c <- extract x
    (t , substitute) <- unify c (Par a b)

    return (substituteP ++ substitute, Map.insert (toAbstract x) t sessionP)

  SelectL x p _ -> do

    -- infer P
    a <- freshType
    b <- freshType
    (substituteP, sessionP) <- inferWith p $ pairs [(x, a)]

    c <- extract x
    (t , substitute) <- unify c (Plus a b)

    return (substituteP ++ substitute, Map.insert (toAbstract x) t sessionP)

  SelectR x p _ -> do

    -- infer P
    b <- freshType
    a <- freshType
    (substituteP, sessionP) <- inferWith p $ pairs [(x, b)]

    c <- extract x
    (t , substitute) <- unify c (Plus a b)

    return (substituteP ++ substitute, Map.insert (toAbstract x) t sessionP)

  Choice x p q _ -> do

    a <- freshType
    (substituteP, sessionP) <- inferWith p $ pairs [(x, a)]

    b <- freshType
    (substituteQ, sessionQ) <- inferWith q $ pairs [(x, b)]

    c <- extract x
    (t , substitute) <- unify c (With a b)

    sessionShouldBeTheSame term sessionP sessionQ

    return (substituteP ++ substituteQ ++ substitute, Map.insert (toAbstract x) t sessionP)

  Accept x y p _ -> do

    a <- freshType
    (substituteP, sessionP) <- inferWith p $ pairs [(y, a)]

    sessionShouldAllBeRequesting term sessionP

    a' <- extract x
    (t , substitute) <- unify a' (Acc a)

    return (substituteP ++ substitute, Map.insert (toAbstract x) t sessionP)

  Request x y p _ -> do

    a <- freshType
    (substituteP, sessionP) <- inferWith p $ pairs [(y, a)]

    a' <- extract x
    (t , substitute) <- unify a' (Req a)

    return (substituteP ++ substitute, Map.insert (toAbstract x) t sessionP)

  OutputT x outputType p _ -> do


    afterSubstitution <- freshType    -- B {A / X}
    beforeSubstitution <- freshType   -- B
    (substituteP, sessionP) <- inferWith p $ pairs [(x, afterSubstitution)]

    body <- extract x
    (body' , substitute) <- unify
      body
      (Exists
                Unknown             -- the type variable to be substituted
                beforeSubstitution  -- the type before substitution
                (Just
                  ( toAbstract outputType   -- the type to be substituted with
                  , afterSubstitution       -- the resulting type after substitution
                  )))

    return (substituteP ++ substitute, Map.insert (toAbstract x) body' sessionP)


  InputT x var p _ -> do

    b <- freshType
    (substituteP, sessionP) <- inferWith p $ pairs [(x, b)]

    t <- extract x
    (t' , substitute) <- unify t (Forall (toAbstract var) b)

    return (substituteP ++ substitute, Map.insert (toAbstract x) t' sessionP)


  EmptyOutput x _ -> do

    t <- extract x
    (t', substitute) <- unify One t

    return (substitute, Map.empty)

  EmptyInput x p _ -> do

    t <- extract x
    (t', substitute) <- unify Bot t

    (substituteP, sessionP) <- inferWith p Map.empty


    return (substitute ++ substituteP, sessionP)

  EmptyChoice x _ -> do

    t <- extract x
    (t', substitute) <- unify Top t

    return (substitute, Map.empty)

  End _ -> do

    -- sessionShouldBeEmpty
    return ([], Map.empty)


  _ -> return ([], Map.empty)

  where
    -- from the input session
    extract :: Chan -> InferM Type
    extract chan = case Map.lookup (toAbstract chan) input of
      Nothing -> do
        t <- freshType
        -- emitting free variable
        modify (Map.insert (toAbstract chan) t)
        return t
      Just t  -> return t

    -- taking extra care when unifying two opposite types
    -- because we might will lose something when taking the dual of (Exists _ _ _)
    unifyOpposite :: Type -> Type -> InferM (Type, [Substitution])
    unifyOpposite a@(Exists _ _ _) b@(Forall _ _) = unify a (dual b)
    unifyOpposite a@(Forall _ _) b@(Exists _ _ _) = unify (dual a) b
    unifyOpposite a b                             = unify a (dual b)

    -- unify the two given types, and update the give session.
    -- for better error message, make the former type be the expecting type
    -- and the latter be the given type
    unify :: Type -> Type -> InferM (Type, [Substitution])
    unify expected given = do
      let (result, subst) = U.unify expected given
      case result of
        Left (t, u) -> throwError $ InferError $ TypeMismatch term expected given t u
        Right t -> return (t, subst)

    freshType :: InferM Type
    freshType = do
        i <- lift $ gets stTypeCount
        lift $ modify $ \ st -> st { stTypeCount = i + 1 }
        return $ Var $ Nameless i

    dualSession :: Session -> Session
    dualSession = fmap dual

    pairs :: [(C.TermName Loc, Type)] -> Session
    pairs = Map.fromList . map (\(c, t) -> (toAbstract c, t))


  -- where
  --   execSubstituton :: Session -> [Substitution] -> Session

--
