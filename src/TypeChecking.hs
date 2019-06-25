module TypeChecking where

import qualified Syntax.Binding as B
import qualified Syntax.Concrete as C
-- import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
import TypeChecking.Infer
import TypeChecking.Binding
import TypeChecking.Base
import Base
--
import Prelude hiding (lookup)


import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Control.Monad.State
import Control.Monad.Except


scopeCheck :: C.Program -> M (Map B.Name B.Definition)
scopeCheck program = do
  -- check the program for duplicated definitions
  checkDuplications program
  -- form the binding structure
  return $ bindProgram program

typeCheck :: Map B.Name B.Definition -> TCM (Map B.Name B.Session)
typeCheck definitions = do
  Map.traverseMaybeWithKey typeCheckOrInfer definitions
  where
    typeCheckOrInfer :: B.Name -> B.Definition -> TCM (Maybe B.Session)
    typeCheckOrInfer _ (B.Annotated name term session) = do
      _ <- check name session term
      return Nothing
    typeCheckOrInfer _ (B.Unannotated _ term) =
      inferTerm term >>= return . Just


--------------------------------------------------------------------------------

-- there should be only at most one type signature or term definition
checkDuplications :: C.Program -> M ()
checkDuplications (C.Program declarations _) = do
  let typeSigNames = mapMaybe C.typeSigName declarations
  let termDefnNames = mapMaybe C.termDefnName declarations

  case getDuplicatedPair typeSigNames of
    Nothing     -> return ()
    Just (a, b) -> throwError $ ScopeError $ TypeSigDuplicated a b
  case getDuplicatedPair termDefnNames of
    Nothing     -> return ()
    Just (a, b) -> throwError $ ScopeError $ TermDefnDuplicated a b

  where
    getDuplicatedPair :: [C.Name] -> Maybe (C.Name, C.Name)
    getDuplicatedPair names =
      let dup = filter ((> 1) . length) $ List.group $ List.sort names
      in if null dup then Nothing else Just (head dup !! 0, head dup !! 1)

-- scopeCheckAll :: Program -> M ()
-- scopeCheckAll program = do
--   -- checking the definitions
--   checkDuplications program
--   -- store the definitions
--   putDefinitions program

-- bindingCheckAll :: TCM (Map Name Definition)
-- bindingCheckAll = bindDefinitions <$> gets stConcreteDefns
   -- bind definitions
  -- forM_ definitions (bindingCheck . C.extractProcess)
  -- Map.traverseWithKey (const $ bindingCheck . extractProcess) definitions
--
