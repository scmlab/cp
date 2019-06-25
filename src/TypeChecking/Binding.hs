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

--
-- freeChannels :: Process -> Set Chan
-- freeChannels (Call ) = freeChannels p `Set.union` freeChannels q
-- freeChannels (Compose x _ p q _) = freeChannels p `Set.union` freeChannels q



bindingCheck :: B.Process -> TCM ()
bindingCheck process = error $ show process

--------------------------------------------------------------------------------
-- | Converting to Concrete Binding Tree

data Binding = Binding
  { bindingBound :: Int
  , bindingFree :: Set Text
  } deriving (Show)

data BindingState = BindingState
  { bsChannel :: Binding
  , bsTypeVar :: Binding
  } deriving (Show)

type BindingM = StateT BindingState (Reader (Map B.Name Definition))

class ToBinding a b | a -> b where
  toBindingM :: a -> BindingM b

toBinding :: ToBinding a b => Map B.Name Definition -> a -> b
toBinding definitions x =
  runReader
    (evalStateT
      (toBindingM x)
      (BindingState (Binding 0 Set.empty) (Binding 0 Set.empty)))
    definitions

--------------------------------------------------------------------------------

createBinderTypeVar :: TypeVar -> BindingM (B.TypeVar, B.Type -> B.Type)
createBinderTypeVar (TypeVar name loc) = do
  Binding idx ns <- gets bsTypeVar
  modify $ \ st -> st { bsTypeVar = Binding (idx + 1) ns }
  let var = B.TypeVar (Bound idx) name loc
  return (var, B.subsituteType name idx)

instance ToBinding TypeVar B.TypeVar where
  toBindingM (TypeVar name loc) = do
    Binding idx ns <- gets bsTypeVar
    modify $ \ st -> st { bsTypeVar = Binding idx (Set.insert name ns) }
    return $ B.TypeVar (Free name) name loc

instance ToBinding Type B.Type where
  toBindingM (Var i loc) =
    B.Var
      <$> toBindingM i
      <*> pure loc
  toBindingM (Dual t loc) =
    B.Dual
      <$> toBindingM t
      <*> pure loc
  toBindingM (Times t u loc) =
    B.Times
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (Par t u loc) =
    B.Par
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (Plus t u loc) =
    B.Plus
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (With t u loc) =
    B.With
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (Acc t loc) =
    B.Acc
      <$> toBindingM t
      <*> pure loc
  toBindingM (Req t loc) =
    B.Req
      <$> toBindingM t
      <*> pure loc
  toBindingM (Exists x t loc) = do
    (var, bindFreeVars) <- createBinderTypeVar x
    t' <- bindFreeVars <$> toBindingM t
    return $ B.Exists var t' Nothing loc
  toBindingM (Forall x t loc) = do
    (var, bindFreeVars) <- createBinderTypeVar x
    t' <- bindFreeVars <$> toBindingM t
    return $ B.Forall var t' loc
  toBindingM (One loc) = B.One <$> pure loc
  toBindingM (Bot loc) = B.Bot <$> pure loc
  toBindingM (Zero loc) = B.Zero <$> pure loc
  toBindingM (Top loc) = B.Top <$> pure loc

--------------------------------------------------------------------------------

-- creates a channcel binder along with a function that binds free variables
createBinderChan :: Chan -> BindingM (B.Chan, B.Process -> B.Process)
createBinderChan (Chan name loc) = do
  Binding idx ns <- gets bsChannel
  modify $ \ st -> st { bsChannel = Binding (idx + 1) ns }
  let var = B.Chan (Bound idx) name loc
  return (var, B.subsituteProcess name idx)


instance ToBinding TypeName B.TypeName where
  toBindingM (TypeName name loc) = return $ B.TypeName name loc

instance ToBinding Name B.Name where
  toBindingM (Name name loc) = return $ B.Name name loc

instance ToBinding Chan B.Chan where
  toBindingM (Chan name loc) = do
    Binding idx ns <- gets bsChannel
    modify $ \ st -> st { bsChannel = Binding idx (Set.insert name ns) }
    return $ B.Chan (Free name) name loc

instance ToBinding Process B.Process where
  toBindingM (Call name loc) =
    B.Call
      <$> toBindingM name
      <*> pure loc
  toBindingM (Link nameA nameB loc) =
    B.Link
      <$> toBindingM nameA
      <*> toBindingM nameB
      <*> pure loc
  toBindingM (Compose x Nothing procA procB loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> pure Nothing
      <*> (bind <$> toBindingM procA)
      <*> (bind <$> toBindingM procB)
      <*> pure loc
  toBindingM (Compose x (Just t) procA procB loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> (Just <$> toBindingM t)
      <*> (bind <$> toBindingM procA)
      <*> (bind <$> toBindingM procB)
      <*> pure loc
  toBindingM (Output nameA nameB procB procA loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Output
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM procB)
      <*> toBindingM procA
      <*> pure loc
  toBindingM (Input nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Input
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM proc)
      <*> pure loc
  toBindingM (SelectL name proc loc) =
    B.SelectL
      <$> toBindingM name
      <*> toBindingM proc
      <*> pure loc
  toBindingM (SelectR name proc loc) =
    B.SelectR
      <$> toBindingM name
      <*> toBindingM proc
      <*> pure loc
  toBindingM (Choice name procA procB loc) =
    B.Choice
      <$> toBindingM name
      <*> toBindingM procA
      <*> toBindingM procB
      <*> pure loc
  toBindingM (Accept nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Accept
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM proc)
      <*> pure loc
  toBindingM (Request nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Request
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM proc)
      <*> pure loc
  toBindingM (OutputT name typ proc loc) =
    B.OutputT
      <$> toBindingM name
      <*> toBindingM typ
      <*> toBindingM proc
      <*> pure loc
  toBindingM (InputT name (TypeVar n l) proc loc) = do
    B.InputT
      <$> toBindingM name
      <*> pure (B.TypeVar (Free n) n l)
      <*> toBindingM proc
      <*> pure loc
  toBindingM (EmptyOutput name loc) =
    B.EmptyOutput
      <$> toBindingM name
      <*> pure loc
  toBindingM (EmptyInput name proc loc) =
    B.EmptyInput
      <$> toBindingM name
      <*> toBindingM proc
      <*> pure loc
  toBindingM (EmptyChoice name loc) =
    B.EmptyChoice
      <$> toBindingM name
      <*> pure loc
  toBindingM (End loc) =
    B.End
      <$> pure loc
  toBindingM (Mix p q loc) =
    B.Mix
      <$> toBindingM p
      <*> toBindingM q
      <*> pure loc

--------------------------------------------------------------------------------

instance ToBinding Program B.Program where
  toBindingM (Program declarations loc) =
    B.Program
      <$> mapM toBindingM declarations
      <*> pure loc

instance ToBinding Declaration B.Declaration where
  toBindingM (TypeSig name session loc) =
    B.TypeSig
      <$> toBindingM name
      <*> toBindingM session
      <*> pure loc
  toBindingM (TermDefn name process loc) =
    B.TermDefn
      <$> toBindingM name
      <*> toBindingM process
      <*> pure loc

instance ToBinding SessionSyntax B.SessionSyntax where
  toBindingM (SessionSyntax pairs loc) = do
    let (keys, elems) = unzip $ Map.toList pairs
    keys' <- mapM toBindingM keys
    elems' <- mapM toBindingM elems
    let pairs' = Map.fromList (zip keys' elems')
    return $ B.SessionSyntax pairs' loc
