module TypeChecking where


import Syntax.Concrete
import qualified Syntax.TypeChecking as TC
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
  , stEnv       :: Map (TermName Loc) TC.Type
  } deriving (Show)

initialTCState :: TCState
initialTCState = TCState Map.empty Map.empty Map.empty

data TypeError = TypeSigDuplicated (TermName Loc) (TermName Loc)
               | TermDefnDuplicated (TermName Loc) (TermName Loc)
               | TypeSigNotFound (TermName Loc)
               | TermDefnNotFound (TermName Loc)
               | Others Text
    deriving (Show)

type TCM = ExceptT TypeError (State TCState)

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

addToEnv :: TermName Loc -> TC.Type -> TCM ()
addToEnv var t = modify $ \ st -> st { stEnv = Map.insert var t (stEnv st) }

--------------------------------------------------------------------------------
-- | All kinds of checkings

-- checkAll :: Program Loc -> TCM ()
checkAll program = do
  -- checking the definitions
  checkDuplications program
  checkTypeTermPairing program
  -- store the definitions
  putTypeSigs program
  putTermDefns program
  --
  -- termDefns <- Map.toList <$> gets stTermDefns
  -- forM termDefns $ \ (var, term) -> do
  --   infer term

  -- gets stEnv


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
-- Link      (TermName ann) (TermName ann)                 ann
--                   | Compose   (TermName ann) (Process  ann) (Process ann)   ann
--                   | Output    (TermName ann) (TermName ann) (Process ann) (Process ann) ann
--                   | Input     (TermName ann) (TermName ann) (Process ann)   ann
--                   | SelectL   (TermName ann) (Process  ann)                 ann
--                   | SelectR   (TermName ann) (Process  ann)                 ann
--                   | Choice    (TermName ann) (Process  ann) (Process ann)   ann
--                   | Accept    (TermName ann) (TermName ann) (Process ann)   ann
--                   | Request   (TermName ann) (TermName ann) (Process ann)   ann
--                   | EmptyOutput              (TermName ann)                 ann
--                   | EmptyInput               (TermName ann) (Process ann)   ann
--                   | EmptyChoice
freeVariable :: Process Loc -> Set (TermName Loc)
freeVariable (Link x y _) = Set.fromList [x, y]
freeVariable (Compose x p q _) = Set.delete x $ Set.union (freeVariable p) (freeVariable q)
freeVariable (Output x y p q _) = Set.insert x $ Set.delete y $ Set.union (freeVariable p) (freeVariable q)
freeVariable (Input x y p _) = Set.insert x $ Set.delete y (freeVariable p)
freeVariable (SelectL x p _) = Set.insert x $ freeVariable p
freeVariable (SelectR x p _) = Set.insert x $ freeVariable p
freeVariable (Choice x p q _) = Set.insert x $ Set.union (freeVariable p) (freeVariable q)
freeVariable (Accept x y p _) = Set.insert x $ Set.delete y (freeVariable p)
freeVariable (Request x y p _) = Set.insert x $ Set.delete y (freeVariable p)
freeVariable (EmptyOutput x _) = Set.singleton x
freeVariable (EmptyInput x p _) = Set.insert x $ freeVariable p
freeVariable (EmptyChoice x _) = Set.singleton x



infer :: Process Loc -> TCM TC.Type
infer (EmptyOutput var _) = return TC.One
    -- addToEnv var TC.One
infer (Output x y p q _) = do
  t <- infer p
  u <- infer q
  return (TC.Times t u)
  -- Map.insert var TC.One Map.empty
infer _ = undefined

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
-- | Typing Environment

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
