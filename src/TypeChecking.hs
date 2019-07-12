module TypeChecking where

import qualified Syntax.Binding as B
import qualified Syntax.Concrete as C
-- import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
-- import TypeChecking.Infer
import TypeChecking.Infer (inferProcess, check)
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
import Pretty

bind :: Bind a b => Maybe C.Program -> a -> M b
bind program process = do
  let result = runBindM (maybe Map.empty C.toDefinitions program) (bindM process)
  case result of
    Left err -> throwError $ ScopeError err
    Right val -> return val

scopeCheck :: C.Program -> M B.Definitions
scopeCheck program = do
  -- check the program for duplicated definitions
  checkDuplications program
  -- check the program for recursive calls
  checkRecursion program
  -- form the binding structure
  B.Program definitions _ <- bind (Just program) program
  modify $ \ st -> st { replDefinitions = definitions }

  return definitions


typeCheck :: B.Definitions -> TCM (Map B.Name B.Session)
typeCheck definitions = do
  Map.traverseMaybeWithKey typeCheckOrInfer definitions
  where
    typeCheckOrInfer :: B.Name -> B.Definition -> TCM (Maybe B.Session)
    typeCheckOrInfer _ (B.Paired name process session) = do
      _ <- check name process session
      return Nothing
    typeCheckOrInfer _ (B.TermOnly _ process) =
      inferProcess process >>= return . Just
    typeCheckOrInfer _ (B.TypeOnly _ session) = return $ Just session


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

checkRecursion :: C.Program -> M ()
checkRecursion program = do
  case C.detectLoop (C.buildCallGraph program) of
    Nothing -> return ()
    Just loop -> throwError $ ScopeError $ RecursiveCall loop

-- scopeCheckAll :: Program -> M ()
-- scopeCheckAll program = do
--   -- checking the definitions
--   checkDuplications program
--   -- store the definitions
--   putDefinitions program

-- bindingCheckAll :: TCM (Map Name Definition)
-- bindingCheckAll = bindDefinitions <$> gets stConcreteDefns
   -- bind definitions
  -- forM_ definitions (bindingCheck . C.toProcess)
  -- Map.traverseWithKey (const $ bindingCheck . toProcess) definitions
--
