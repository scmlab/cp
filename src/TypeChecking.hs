module TypeChecking where

import Syntax.Binding
-- import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
import TypeChecking.Infer
import TypeChecking.Binding
import TypeChecking.Base
--
import Prelude hiding (lookup)


import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Control.Monad.State
import Control.Monad.Except


checkAll :: Program -> TCM (Map Name Session)
checkAll program = do
  scopeCheckAll program
  bindingCheckAll
  typeCheckAll


--------------------------------------------------------------------------------

-- there should be only at most one type signature or term definition
checkDuplications :: Program -> TCM ()
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
    getDuplicatedPair :: [Name] -> Maybe (Name, Name)
    getDuplicatedPair names =
      let dup = filter ((> 1) . length) $ List.group $ List.sort names
      in if null dup then Nothing else Just (head dup !! 0, head dup !! 1)

scopeCheckAll :: Program -> TCM ()
scopeCheckAll program = do
  -- checking the definitions
  checkDuplications program
  -- store the definitions
  putDefiniotions program

extractProcess :: Definition -> Process
extractProcess (Annotated _ term _) = term
extractProcess (Unannotated _ term) = term

bindingCheckAll :: TCM ()
bindingCheckAll = do
  definitions <- Map.elems <$> gets stDefinitions
  forM_ definitions (bindingCheck . extractProcess)
  -- Map.traverseWithKey (const $ bindingCheck . extractProcess) definitions

typeCheckAll :: TCM (Map Name Session)
typeCheckAll = do
  -- return the inferred definitions
  definitions <- gets stDefinitions
  Map.traverseMaybeWithKey typeCheckOrInfer definitions


typeCheckOrInfer :: Name -> Definition -> TCM (Maybe Session)
typeCheckOrInfer _ (Annotated name term session) = do
  _ <- check name session term
  return Nothing
typeCheckOrInfer _ (Unannotated _ term) =
  inferTerm term >>= return . Just

putDefiniotions :: Program -> TCM ()
putDefiniotions (Program declarations _) =
  modify $ \ st -> st { stDefinitions = definitions }
  where
    toTypeSigPair (TypeSig n s _) = Just (n, convert s)
    toTypeSigPair _               = Nothing

    toTermDefnPair (TermDefn n t _) = Just (n, (n, t))
    toTermDefnPair _                = Nothing

    typeSigs :: Map Name Session
    typeSigs  = Map.fromList $ mapMaybe toTypeSigPair declarations

    termDefns :: Map Name (Name, Process)
    termDefns = Map.fromList $ mapMaybe toTermDefnPair declarations

    termsWithTypes :: Map Name Definition
    termsWithTypes = Map.map (\ ((n, t), s) -> Annotated n t s) $ Map.intersectionWith (,) termDefns typeSigs

    termsWithoutTypes :: Map Name Definition
    termsWithoutTypes = Map.map (\ (n, t) -> Unannotated n t) $ Map.difference termDefns typeSigs

    definitions :: Map Name Definition
    definitions = Map.union termsWithTypes termsWithoutTypes
