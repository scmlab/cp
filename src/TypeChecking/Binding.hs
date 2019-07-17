{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module TypeChecking.Binding
  ( runBindM
  , Bind(..)
  ) where

import Syntax.Base
import Syntax.Concrete
import qualified Syntax.Binding as B
import TypeChecking.Base

import Data.Loc (Loc(..))
import Data.Text (Text)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (LT, EQ, GT)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

-- import Debug.Trace

--------------------------------------------------------------------------------
-- | Converting to Concrete Binding Tree

data BindingState = BindingState
  { bsFreeChans     :: Set Text
  , bsFreeTypeVars  :: Set Text
  } deriving (Show)

type BindM = ExceptT ScopeError (StateT BindingState (Reader Definitions))

class Bind a b | a -> b where
  bindM :: a -> BindM b

runBindM :: Definitions -> BindM a -> Either ScopeError a
runBindM defns f =
  runReader
    (evalStateT
      (runExceptT
        f)
      initialBindingState)
    defns
  where
    initialBindingState :: BindingState
    initialBindingState = BindingState Set.empty Set.empty

askDefn :: Name -> Loc -> BindM Definition
askDefn name loc = do
  result <- Map.lookup name <$> ask
  case result of
    Nothing -> throwError $ DefnNotFound (Call name loc) name
    Just definition -> return definition


--------------------------------------------------------------------------------

instance Bind TypeVar B.TypeVar where
  bindM (TypeVar name loc) = do
    names <- gets bsFreeTypeVars
    modify $ \ st -> st { bsFreeTypeVars = Set.insert name names }
    return $ B.TypeVar name loc

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
    -- binder
    B.Exists
      <$> bindM x
      <*> bindM t
      <*> pure Nothing
      <*> pure loc
  bindM (Forall x t loc) = do
    -- binder
    B.Forall
      <$> bindM x
      <*> bindM t
      <*> pure loc
  bindM (One loc) = B.One <$> pure loc
  bindM (Bot loc) = B.Bot <$> pure loc
  bindM (Zero loc) = B.Zero <$> pure loc
  bindM (Top loc) = B.Top <$> pure loc

--------------------------------------------------------------------------------

instance Bind Name B.Name where
  bindM (Name name loc) = return $ B.Name name loc

instance Bind TypeName B.TypeName where
  bindM (TypeName name loc) = return $ B.TypeName name loc

instance Bind Chan B.Chan where
  bindM (Chan name loc) = do
    names <- gets bsFreeChans
    modify $ \ st -> st { bsFreeChans = Set.insert name names }
    return $ B.Chan name loc

instance Bind Process B.Process where
  bindM (Call name loc) = do
    defn <- askDefn name loc
    result <- case defn of
                  Paired _ p _ -> Right <$> bindM p
                  TypeOnly _ s -> return $ Left $ sessionKeys s
                  TermOnly _ p -> Right <$> bindM p
    B.Call
      <$> bindM name
      <*> pure result
      <*> pure loc
  bindM (Link x y loc) =
    B.Link
      <$> bindM x
      <*> bindM y
      <*> pure loc
  bindM (Compose x t p q loc) = do
    B.Compose
      -- binder
      <$> bindM x
      <*> mapM bindM t
      <*> bindM p
      <*> bindM q
      <*> pure loc
  bindM (Output x y q p loc) = do
    B.Output
      <$> bindM x
      -- binder
      <*> bindM y
      <*> bindM q
      <*> bindM p
      <*> pure loc
  bindM (Input x y p loc) = do
    B.Input
      <$> bindM x
      -- binder
      <*> bindM y
      <*> bindM p
      <*> pure loc
  bindM (SelectL x p loc) =
    B.SelectL
      <$> bindM x
      <*> bindM p
      <*> pure loc
  bindM (SelectR x p loc) =
    B.SelectR
      <$> bindM x
      <*> bindM p
      <*> pure loc
  bindM (Choice x p q loc) =
    B.Choice
      <$> bindM x
      <*> bindM p
      <*> bindM q
      <*> pure loc
  bindM (Accept x y p loc) = do
    B.Accept
      <$> bindM x
      -- binder
      <*> bindM y
      <*> bindM p
      <*> pure loc
  bindM (Request x y p loc) = do
    B.Request
      <$> bindM x
      -- binder
      <*> bindM y
      <*> bindM p
      <*> pure loc
  bindM (OutputT x t p loc) =
    B.OutputT
      <$> bindM x
      <*> bindM t
      <*> bindM p
      <*> pure loc
  bindM (InputT x (TypeVar n l) p loc) = do
    B.InputT
      <$> bindM x
      <*> pure (B.TypeVar n l)
      <*> bindM p
      <*> pure loc
  bindM (EmptyOutput x loc) =
    B.EmptyOutput
      <$> bindM x
      <*> pure loc
  bindM (EmptyInput x p loc) =
    B.EmptyInput
      <$> bindM x
      <*> bindM p
      <*> pure loc
  bindM (EmptyChoice x loc) =
    B.EmptyChoice
      <$> bindM x
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
      <$> bindM (toDefinitions (Program declarations loc))
      <*> pure loc

-- instance Bind Declaration B.Declaration where
--   bindM (TypeSig name session loc) =
--     B.TypeSig
--       <$> bindM name
--       <*> bindM session
--       <*> pure loc
--   bindM (TermDefn name process loc) =
--     B.TermDefn
--       <$> bindM name
--       <*> bindM process
--       <*> pure loc

instance Bind Definition B.Definition where
  bindM (Paired name process session) = do
    B.Paired
      <$> bindM name
      <*> bindM process
      <*> bindM session
  bindM (TypeOnly name session) = do
    B.TypeOnly
      <$> bindM name
      <*> bindM session
  bindM (TermOnly name process) = do
    B.TermOnly
      <$> bindM name
      <*> bindM process

instance Bind Definitions B.Definitions where
  bindM pairs = do
    let (keys, elems) = unzip $ Map.toList pairs
    keys' <- mapM bindM keys
    elems' <- mapM bindM elems
    return $ Map.fromList (zip keys' elems')

instance Bind Session B.Session where
  bindM pairs = do
    let (keys, elems) = unzip $ Map.toList pairs
    keys' <- mapM bindM keys
    elems' <- mapM bindM elems
    return $ Map.fromList (zip keys' elems')

instance Bind SessionSyntax B.SessionSyntax where
  bindM (SessionSyntax pairs loc) =
    B.SessionSyntax
      <$> bindM pairs
      <*> pure loc
