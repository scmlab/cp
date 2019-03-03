module TypeChecker where


import Syntax.Concrete
-- import qualified Syntax.Concrete as C
import Data.Loc (Loc)


import qualified Data.List as List
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

data TCState = TCState
    { stateTypeSigs  :: Map (TermName Loc) (Type Loc)
    , stateTermDefns :: Map (TermName Loc) (Process Loc)
    } deriving (Show)

initialTCState :: TCState
initialTCState = TCState Map.empty Map.empty

data TypeError = TypeSigDuplicated (TermName Loc) (TermName Loc)
               | TermDefnDuplicated (TermName Loc) (TermName Loc)
               -- TypeSigNotFound (TermName Loc)
               -- | Conflict Variable Type Type
               | Others String
    deriving (Show)

type TCM = ExceptT TypeError (State TCState)


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
        typeSigName (TypeSig n _ _) = Just n
        typeSigName _             = Nothing

        termDefnName (TermDefn n _ _) = Just n
        termDefnName _              = Nothing

        getDuplicatedPair :: [TermName Loc] -> Maybe (TermName Loc, TermName Loc)
        getDuplicatedPair names =
            let dup = filter ((> 1) . length) $ List.group $ List.sort names
            in if null dup then Nothing else Just (head dup !! 0, head dup !! 1)

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


--  (Lexical pos)         source = do
--   setSGR [SetColor Foreground Vivid Red]
--   putStr "\n  Lexical parse error\n  "
--   setSGR [SetColor Foreground Dull Blue]
--   putStrLn $ displayPos pos
--   setSGR []
--   printSourceCode $ SourceCode (BS.unpack source) (Loc pos pos) 2
-- printParseError (Syntatical loc _) (Just source) = do
--   setSGR [SetColor Foreground Vivid Red]
--   putStr "\n  Syntatical parse error\n  "
--   setSGR [SetColor Foreground Dull Blue]
--   putStrLn $ displayLoc loc
--   setSGR []
--   printSourceCode $ SourceCode (BS.unpack source) loc 2
