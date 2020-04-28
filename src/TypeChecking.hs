module TypeChecking where

-- import qualified Syntax.Binding as B
import           Syntax.Concrete
-- import qualified Syntax.Concrete as C
-- import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
-- import TypeChecking.Infer
import           TypeChecking.Infer             ( inferProcess
                                                , check
                                                )
-- import TypeChecking.Binding
import           TypeChecking.Base
import           Base
--
import           Prelude                 hiding ( lookup )


import qualified Data.List                     as List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( (\\) )
import qualified Data.Set                      as Set
import           Data.Maybe                     ( mapMaybe )

import           Control.Monad.State
import           Control.Monad.Except
-- import Pretty

-- bind :: Bind a b => Maybe Program -> a -> M b
-- bind program process = do
--   let result = runBindM (maybe Map.empty toDefinitions program) (bindM process)
--   case result of
--     Left err -> throwError $ ScopeError err
--     Right val -> return val

scopeCheck :: Program -> M Definitions
scopeCheck program = do
  -- check the program for duplicated definitions
  checkDuplications program

  let definitions = toDefinitions program

  let callGraph   = buildCallGraph definitions
  -- see if there are recursive calls
  checkRecursiveCalls callGraph
  -- see if there are out-of-scope calls
  checkOutOfScopeCalls callGraph


  -- store and return the checked definitions
  modify $ \st -> st { replDefinitions = definitions }
  return definitions

checkOutOfScope :: Process -> M ()
checkOutOfScope process = do
  definitions <- gets replDefinitions
  let callGraph        = buildCallGraph definitions
  let topLevelBindings = Map.keysSet callGraph
  let calls            = collectCalls process
  let outOfScopeCalls  = calls \\ topLevelBindings
  case Set.lookupMin outOfScopeCalls of
    Nothing -> return ()
    Just n  -> throwError $ ScopeError $ DefnNotFound n

-- -- see if the names in a process is in the scope
-- scopeCheckProcess :: Process -> M ()
-- scopeCheckProcess process = case process of
--   Call name _ -> do
--     definitions <- gets replDefinitions
--     if Map.member name definitions
--       then return ()
--       else throwError $ ScopeError $ DefnNotFound name
--   Link _ _ _        -> return ()
--   Compose _ _ p q _ -> do
--     scopeCheckProcess p
--     scopeCheckProcess q
--   Output _ _ p q _ -> do
--     scopeCheckProcess p
--     scopeCheckProcess q
--   Input _ _ p _  -> scopeCheckProcess p
--   SelectL _ p _  -> scopeCheckProcess p
--   SelectR _ p _  -> scopeCheckProcess p
--   Choice _ p q _ -> do
--     scopeCheckProcess p
--     scopeCheckProcess q
--   Accept  _ _ p _  -> scopeCheckProcess p
--   Request _ _ p _  -> scopeCheckProcess p
--   OutputT _ _ p _  -> scopeCheckProcess p
--   InputT  _ _ p _  -> scopeCheckProcess p
--   EmptyOutput _ _  -> return ()
--   EmptyInput _ p _ -> scopeCheckProcess p
--   EmptyChoice _ _  -> return ()
--   End _            -> return ()
--   Mix p q _        -> do
--     scopeCheckProcess p
--     scopeCheckProcess q

typeCheck :: Definitions -> TCM (Map Name Session)
typeCheck = Map.traverseMaybeWithKey typeCheckOrInfer
 where
  typeCheckOrInfer :: Name -> Definition -> TCM (Maybe Session)
  typeCheckOrInfer _ (Paired name process session) = do
    _ <- check name process session
    return Nothing
  typeCheckOrInfer _ (TermOnly _ process) =
    inferProcess process >>= return . Just
  typeCheckOrInfer _ (TypeOnly _ session) = return $ Just session


--------------------------------------------------------------------------------

-- there should be only at most one type signature or term definition
checkDuplications :: Program -> M ()
checkDuplications (Program declarations _) = do
  let typeSigNames  = mapMaybe typeSigName declarations
  let termDefnNames = mapMaybe termDefnName declarations

  case getDuplicatedPair typeSigNames of
    Nothing     -> return ()
    Just (a, b) -> throwError $ ScopeError $ TypeSigDuplicated a b
  case getDuplicatedPair termDefnNames of
    Nothing     -> return ()
    Just (a, b) -> throwError $ ScopeError $ TermDefnDuplicated a b

 where
  getDuplicatedPair :: [Name] -> Maybe (Name, Name)
  getDuplicatedPair names =
    let dup = filter ((> 1) . length) $ List.group $ List.sort names
    in  if null dup then Nothing else Just (head dup !! 0, head dup !! 1)

checkRecursiveCalls :: CallGraph -> M ()
checkRecursiveCalls callGraph = case detectLoop callGraph of
  Nothing   -> return ()
  Just loop -> throwError $ ScopeError $ RecursiveCall loop

checkOutOfScopeCalls :: CallGraph -> M ()
checkOutOfScopeCalls callGraph = case detectOutOfScope callGraph of
  Nothing   -> return ()
  Just name -> throwError $ ScopeError $ DefnNotFound name
