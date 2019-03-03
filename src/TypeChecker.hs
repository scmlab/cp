module TypeChecker where


import Syntax.Abstract
import Data.List (sort, group)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

data TCState = TCState
    { stateTypeSigs  :: Map TermName Type
    , stateTermDefns :: Map TermName Process
    } deriving (Show)

initialTCState :: TCState
initialTCState = TCState Map.empty Map.empty

data TypeError = TypeSigNotFound TermName
               | TypeSigDuplicated TermName
               | TermDefnDuplicated TermName
               -- | Conflict Variable Type Type
               | Others String
    deriving (Show)

type TCM = ExceptT TypeError (State TCState)


-- there should be only at most one type signature or term definition
ruleOutDuplications :: Program -> TCM ()
ruleOutDuplications (Program declarations) = do
    let typeSigNames = mapMaybe typeSigName declarations
    let termDefnNames = mapMaybe termDefnName declarations

    let duplicatedTypeSigs = filter ((> 1) . length) $ group $ sort typeSigNames
    let duplicatedTermDefns = filter ((> 1) . length) $ group $ sort termDefnNames

    when (length duplicatedTypeSigs > 0) $ do
        throwError $ TypeSigDuplicated $ head $ head duplicatedTypeSigs
    when (length duplicatedTermDefns > 0) $ do
        throwError $ TermDefnDuplicated $ head $ head duplicatedTermDefns
    -- let duplicatedTermDefn = length $ filter (> 1) $ map length $ group $ sort typeSigNames

    return ()

    where
        typeSigName (TypeSig n _) = Just n
        typeSigName _             = Nothing

        termDefnName (TermDefn n _) = Just n
        termDefnName _              = Nothing


-- check if a type signature is paired with a term definition,
-- a temporary measure before the type inference algorithm is implelemented
-- checkTypeTermPairing :: Program -> TCM ()
-- checkTypeTermPairing =

-- infer :: Environment -> Process -> Either TypeError Environment
-- infer env (EmptyOutput var) = addToEnv env var One
-- infer env (Times x y p q) = do
--     envA <- infer env p
--     envB <- infer env q
-- -- infer env (SelectL var proc) = do
-- --     env' <- infer env proc
-- --     case Map.lookup var env' of
-- --         Nothing -> Right $ updateEnv env' var (Par Hole Hole)
-- --         Just t  -> Right $ updateEnv env' var (Par t Hole)
-- infer _ _ = Left TypeError
--
--
--------------------------------------------------------------------------------
-- | Environment

-- type Environment =  Map Variable Type

-- emptyEnv :: Environment
-- emptyEnv = Map.empty
--
-- addToEnv :: Environment -> Variable -> Type -> Either TypeError Environment
-- addToEnv env var t = case Map.lookup var env of
--     Just Hole ->
--         Right $ Map.insert var t env
--     Just u -> if normalize t == normalize u
--         then Right env
--         else Left $ Conflict var u t
--     Nothing -> Right $ Map.insert var t env
--
-- updateEnv :: Environment -> Variable -> Type -> Environment
-- updateEnv env var t = Map.insert var t env
