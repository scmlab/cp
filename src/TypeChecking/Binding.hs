{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module TypeChecking.Binding where

import Syntax.Base
import Syntax.Concrete
import qualified Syntax.Binding as B
import TypeChecking.Base

-- import Data.Loc (Loc(..), Located(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (LT, EQ, GT)

import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.Except


freeChannels :: Process -> BindM (Set B.Chan)
freeChannels (Call name _) = do
  result <- Map.lookup name <$> gets bsCallees
  case result of
    Nothing -> do
      lookupResult <- Map.lookup name <$> ask
      case lookupResult of
        Nothing -> undefined
        Just defn -> freeChannels (extractProcess defn)
    Just (B.Callee _ s) -> return s
freeChannels (Compose x _ p q _) =
  Set.union
    <$> freeChannels p
    <*> freeChannels q


bindProgram :: Program -> Map B.Name B.Definition
bindProgram = undefined . toDefinitions

--------------------------------------------------------------------------------
-- | Converting to Concrete Binding Tree

data Binding = Binding
  { bindingBound :: Int
  , bindingFree :: Set Text
  } deriving (Show)

data BindingState = BindingState
  { bsChannel :: Binding
  , bsTypeVar :: Binding
  , bsCallees :: Map Name B.Callee
  } deriving (Show)

type Definitions = Map Name Definition
type BindM = StateT BindingState (Reader Definitions)

class Bind a b | a -> b where
  bindM :: a -> BindM b

bind :: Bind a b => Maybe Program -> a -> b
bind program x =
  runReader
    (evalStateT
      (bindM x)
      initialBindingState)
    (maybe Map.empty toDefinitions program)
  where
    initialBindingState :: BindingState
    initialBindingState =
      BindingState
        (Binding 0 Set.empty)
        (Binding 0 Set.empty)
        Map.empty

-- lookupCallee :: Name -> Maybe

--------------------------------------------------------------------------------

createBinderTypeVar :: TypeVar -> BindM (B.TypeVar, B.Type -> B.Type)
createBinderTypeVar (TypeVar name loc) = do
  Binding idx ns <- gets bsTypeVar
  modify $ \ st -> st { bsTypeVar = Binding (idx + 1) ns }
  let var = B.TypeVar (Bound idx) name loc
  return (var, B.subsituteType name idx)

instance Bind TypeVar B.TypeVar where
  bindM (TypeVar name loc) = do
    Binding idx ns <- gets bsTypeVar
    modify $ \ st -> st { bsTypeVar = Binding idx (Set.insert name ns) }
    return $ B.TypeVar (Free name) name loc

instance Bind Type B.Type where
  bindM (Var i loc) =
    B.Var
      <$> bindM i
      <*> pure loc
  bindM (Dual t loc) =
    B.Dual
      <$> bindM t
      <*> pure loc
  bindM (Times t u loc) =
    B.Times
      <$> bindM t
      <*> bindM u
      <*> pure loc
  bindM (Par t u loc) =
    B.Par
      <$> bindM t
      <*> bindM u
      <*> pure loc
  bindM (Plus t u loc) =
    B.Plus
      <$> bindM t
      <*> bindM u
      <*> pure loc
  bindM (With t u loc) =
    B.With
      <$> bindM t
      <*> bindM u
      <*> pure loc
  bindM (Acc t loc) =
    B.Acc
      <$> bindM t
      <*> pure loc
  bindM (Req t loc) =
    B.Req
      <$> bindM t
      <*> pure loc
  bindM (Exists x t loc) = do
    (var, bindFreeVars) <- createBinderTypeVar x
    t' <- bindFreeVars <$> bindM t
    return $ B.Exists var t' Nothing loc
  bindM (Forall x t loc) = do
    (var, bindFreeVars) <- createBinderTypeVar x
    t' <- bindFreeVars <$> bindM t
    return $ B.Forall var t' loc
  bindM (One loc) = B.One <$> pure loc
  bindM (Bot loc) = B.Bot <$> pure loc
  bindM (Zero loc) = B.Zero <$> pure loc
  bindM (Top loc) = B.Top <$> pure loc

--------------------------------------------------------------------------------

-- creates a channcel binder along with a function that binds free variables
createBinderChan :: Chan -> BindM (B.Chan, B.Process -> B.Process)
createBinderChan (Chan name loc) = do
  Binding idx ns <- gets bsChannel
  modify $ \ st -> st { bsChannel = Binding (idx + 1) ns }
  let var = B.Chan (Bound idx) name loc
  return (var, B.subsituteProcess name idx)

instance Bind Name B.Name where
  bindM (Name name loc) = return $ B.Name name loc

instance Bind TypeName B.TypeName where
  bindM (TypeName name loc) = return $ B.TypeName name loc

instance Bind Chan B.Chan where
  bindM (Chan name loc) = do
    Binding idx ns <- gets bsChannel
    modify $ \ st -> st { bsChannel = Binding idx (Set.insert name ns) }
    return $ B.Chan (Free name) name loc

-- instance Bind Name B.Callee where
--   bindM (Name name loc) = do
--     B.Callee
--       <$> pure name
--       <*> pure loc

-- throwBack :: B.Name -> Name
-- throwBack (B.Name name loc) = Name name loc

instance Bind Process B.Process where
  bindM (Call name loc) = do
    -- lookup the existing callees
    result <- Map.lookup name <$> gets bsCallees
    callee <- case result of
      -- register a new calee
      Nothing -> do
        lookupResult <- Map.lookup name <$> ask
        case lookupResult of
          Nothing -> undefined
            -- throwError $ InferError $ DefnNotFound (Call name loc) name
          Just definition -> do
            xs <- freeChannels $ extractProcess definition
            B.Callee
              <$> bindM name
              <*> pure xs
              -- (Map.fromList $ map (\n -> (n, Free n)) $ Set.toList xs)
      -- return
      Just c -> return c


    return $ B.Call callee loc
  bindM (Link nameA nameB loc) =
    B.Link
      <$> bindM nameA
      <*> bindM nameB
      <*> pure loc
  bindM (Compose x Nothing procA procB loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> pure Nothing
      <*> (bind <$> bindM procA)
      <*> (bind <$> bindM procB)
      <*> pure loc
  bindM (Compose x (Just t) procA procB loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> (Just <$> bindM t)
      <*> (bind <$> bindM procA)
      <*> (bind <$> bindM procB)
      <*> pure loc
  bindM (Output nameA nameB procB procA loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Output
      <$> bindM nameA
      <*> pure varB
      <*> (bindB <$> bindM procB)
      <*> bindM procA
      <*> pure loc
  bindM (Input nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Input
      <$> bindM nameA
      <*> pure varB
      <*> (bindB <$> bindM proc)
      <*> pure loc
  bindM (SelectL name proc loc) =
    B.SelectL
      <$> bindM name
      <*> bindM proc
      <*> pure loc
  bindM (SelectR name proc loc) =
    B.SelectR
      <$> bindM name
      <*> bindM proc
      <*> pure loc
  bindM (Choice name procA procB loc) =
    B.Choice
      <$> bindM name
      <*> bindM procA
      <*> bindM procB
      <*> pure loc
  bindM (Accept nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Accept
      <$> bindM nameA
      <*> pure varB
      <*> (bindB <$> bindM proc)
      <*> pure loc
  bindM (Request nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Request
      <$> bindM nameA
      <*> pure varB
      <*> (bindB <$> bindM proc)
      <*> pure loc
  bindM (OutputT name typ proc loc) =
    B.OutputT
      <$> bindM name
      <*> bindM typ
      <*> bindM proc
      <*> pure loc
  bindM (InputT name (TypeVar n l) proc loc) = do
    B.InputT
      <$> bindM name
      <*> pure (B.TypeVar (Free n) n l)
      <*> bindM proc
      <*> pure loc
  bindM (EmptyOutput name loc) =
    B.EmptyOutput
      <$> bindM name
      <*> pure loc
  bindM (EmptyInput name proc loc) =
    B.EmptyInput
      <$> bindM name
      <*> bindM proc
      <*> pure loc
  bindM (EmptyChoice name loc) =
    B.EmptyChoice
      <$> bindM name
      <*> pure loc
  bindM (End loc) =
    B.End
      <$> pure loc
  bindM (Mix p q loc) =
    B.Mix
      <$> bindM p
      <*> bindM q
      <*> pure loc

--------------------------------------------------------------------------------

instance Bind Program B.Program where
  bindM (Program declarations loc) =
    B.Program
      <$> mapM bindM declarations
      <*> pure loc

instance Bind Declaration B.Declaration where
  bindM (TypeSig name session loc) =
    B.TypeSig
      <$> bindM name
      <*> bindM session
      <*> pure loc
  bindM (TermDefn name process loc) =
    B.TermDefn
      <$> bindM name
      <*> bindM process
      <*> pure loc

instance Bind SessionSyntax B.SessionSyntax where
  bindM (SessionSyntax pairs loc) = do
    let (keys, elems) = unzip $ Map.toList pairs
    keys' <- mapM bindM keys
    elems' <- mapM bindM elems
    let pairs' = Map.fromList (zip keys' elems')
    return $ B.SessionSyntax pairs' loc
