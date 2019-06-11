module TypeChecking where

import qualified Syntax.Concrete as C
import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
import qualified Syntax.Abstract as A
import TypeChecking.Infer
import TypeChecking.Base
--
import Prelude hiding (lookup)

import Data.Loc (Loc)

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Control.Monad.State
import Control.Monad.Except


--------------------------------------------------------------------------------
-- | TCM

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


checkAll :: Program -> TCM (Map Text A.Session)
checkAll program = do
  -- checking the definitions
  checkDuplications program
  -- store the definitions
  putDefiniotions program

  -- return the inferred definitions
  definitions <- gets stDefinitions
  Map.traverseMaybeWithKey typeCheckOrInfer definitions

typeCheckOrInfer :: Text -> Definition -> TCM (Maybe A.Session)
typeCheckOrInfer key (Annotated name term session) = do
  _ <- typeCheck name (toAbstract session) term
  return Nothing
typeCheckOrInfer _ (Unannotated name term) =
  inferTerm term >>= return . Just

putDefiniotions :: Program -> TCM ()
putDefiniotions (Program declarations _) =
  modify $ \ st -> st { stDefinitions = Map.union termsWithTypes termsWithoutTypes }
  where
    toTypeSigPair (TypeSig n s _) = Just (toAbstract n, s)
    toTypeSigPair _               = Nothing

    toTermDefnPair (TermDefn n t _) = Just (toAbstract n, (n, t))
    toTermDefnPair _                = Nothing

    typeSigs :: Map Text C.Session
    typeSigs  = Map.fromList $ mapMaybe toTypeSigPair declarations

    termDefns :: Map Text (Name, Process)
    termDefns = Map.fromList $ mapMaybe toTermDefnPair declarations

    termsWithTypes :: Map Text Definition
    termsWithTypes = Map.map (\ ((n, t), s) -> Annotated n t s) $ Map.intersectionWith (,) termDefns typeSigs

    termsWithoutTypes :: Map Text Definition
    termsWithoutTypes = Map.map (\ (n, t) -> Unannotated n t) $ Map.difference termDefns typeSigs
