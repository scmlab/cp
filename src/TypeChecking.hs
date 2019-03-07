module TypeChecking where


import TypeChecking.Inference2
import Syntax.Concrete
import Data.Loc (Loc)

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

data TCState = TCState
  { stTypeSigs  :: Map (TermName Loc) (Type Loc)
  , stTermDefns :: Map (TermName Loc) (Process Loc)
  } deriving (Show)

initialTCState :: TCState
initialTCState = TCState Map.empty Map.empty

data TypeError = TypeSigDuplicated (TermName Loc) (TermName Loc)
               | TermDefnDuplicated (TermName Loc) (TermName Loc)
               | TypeSigNotFound (TermName Loc)
               | TermDefnNotFound (TermName Loc)
               | InferError InferError
               -- | ExpectButGot (Type Loc) (Type Loc) Term
               | Others Text
    deriving (Show)

type TCM = ExceptT TypeError (State TCState)

runInferM :: InferM a -> (Either InferError a, InferState)
runInferM program = runState (runExceptT program) initInferState


putTypeSigs :: Program Loc -> TCM ()
putTypeSigs (Program declarations _) = modify $ \ st -> st { stTypeSigs = Map.fromList (mapMaybe toPair declarations) }
  where
    toPair (TypeSig n t _) = Just (n, t)
    toPair _               = Nothing

putTermDefns :: Program Loc -> TCM ()
putTermDefns (Program declarations _) = modify $ \ st -> st { stTermDefns = Map.fromList (mapMaybe toPair declarations) }
  where
    toPair (TermDefn n t _) = Just (n, t)
    toPair _                = Nothing



--------------------------------------------------------------------------------
-- |

-- checkAll :: Program Loc -> TCM [(Either InferError a, EnvState)]
checkAll program = do
  -- checking the definitions
  checkDuplications program
  -- checkTypeTermPairing program
  -- store the definitions
  putTypeSigs program
  putTermDefns program


  -- -- get TypeSig - TermDefn pairs
  -- pairs <- do
  --   typeSigs <- gets stTypeSigs
  --   termDefns <- gets stTermDefns
  --   return $ catMaybes $ map (\(k, v) -> fmap (\t -> (v, t)) $ Map.lookup k termDefns) $ Map.toList typeSigs
  --
  -- -- type check the pairs
  -- forM pairs $ \ (typ, term) -> do
  --   return ()


  -- inference
  termDefns <- Map.toList <$> gets stTermDefns
  forM termDefns $ \ (_, term) -> do
    let (result, s) = runInferM $ inferM term
    case result of
      Left e -> throwError $ InferError e
      Right t -> return (t, s)

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

-- check if a type signature is paired with a term definition,
-- a temporary measure before the type inference algorithm is implelemented
checkTypeTermPairing :: Program Loc -> TCM ()
checkTypeTermPairing (Program declarations _) = do
  let typeSigNames = mapMaybe typeSigName declarations
  let termDefnNames = mapMaybe termDefnName declarations
  let lonelyTypeSigNames = (List.\\) typeSigNames termDefnNames
  let lonelyTermDefnNames = (List.\\) termDefnNames typeSigNames

  unless (null lonelyTypeSigNames) $
    throwError $ TermDefnNotFound (head lonelyTypeSigNames)

  unless (null lonelyTermDefnNames) $
    throwError $ TypeSigNotFound (head lonelyTermDefnNames)

  return ()

freeVariable :: Process Loc -> Set (TermName Loc)
freeVariable (Link x y _) = Set.fromList [x, y]
freeVariable (Compose x _ p q _) = Set.delete x $ Set.union (freeVariable p) (freeVariable q)
freeVariable (Output x y p q _) = Set.insert x $ Set.delete y $ Set.union (freeVariable p) (freeVariable q)
freeVariable (Input x y p _) = Set.insert x $ Set.delete y (freeVariable p)
freeVariable (SelectL x p _) = Set.insert x $ freeVariable p
freeVariable (SelectR x p _) = Set.insert x $ freeVariable p
freeVariable (Choice x p q _) = Set.insert x $ Set.union (freeVariable p) (freeVariable q)
freeVariable (Accept x y p _) = Set.insert x $ Set.delete y (freeVariable p)
freeVariable (Request x y p _) = Set.insert x $ Set.delete y (freeVariable p)
freeVariable (OutputT x _ p _) = Set.insert x (freeVariable p)
freeVariable (InputT x _ p _) = Set.insert x (freeVariable p)
freeVariable (EmptyOutput x _) = Set.singleton x
freeVariable (EmptyInput x p _) = Set.insert x $ freeVariable p
freeVariable (EmptyChoice x _) = Set.singleton x

-- --------------------------------------------------------------------------------
-- -- | Type checking
--
-- typeCheck :: Term -> Type Loc -> TCM ()
-- typeCheck (Link ) t =
--
-- -- typeCheck (Dual t l) term = throwError $ ExpectButGot t (Dual t l) term
-- -- typeCheck (Times t l) (Output ) = throwError $ ExpectButGot t (Dual t l) term
-- typeCheck _ _ = undefined
