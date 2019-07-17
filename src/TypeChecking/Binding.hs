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

type BindM = ExceptT ScopeError (Reader Definitions)

class Bind a b | a -> b where
  bindM :: a -> BindM b

runBindM :: Definitions -> BindM a -> Either ScopeError a
runBindM defns f = runReader (runExceptT f) defns

askDefn :: Name -> Loc -> BindM Definition
askDefn name loc = do
  result <- Map.lookup name <$> ask
  case result of
    Nothing -> throwError $ DefnNotFound (Call name loc) name
    Just definition -> return definition


--------------------------------------------------------------------------------

instance Bind TypeVar B.TypeVar where
  bindM (TypeVar name loc) = return $ B.TypeVar name loc

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
  bindM (Exists x t loc) =
    B.Exists
      <$> bindM x
      <*> bindM t
      <*> pure Nothing
      <*> pure loc
  bindM (Forall x t loc) =
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
  bindM (Chan name loc) = return $ B.Chan name loc

buildProcess :: Loc -> BindM B.Proc -> BindM B.Process
buildProcess loc p = do
  p' <- p
  return $ B.Process p' (B.freeChans' p') loc

instance Bind Process B.Process where
  bindM (Call name loc) = buildProcess loc $ do
    defn <- askDefn name loc
    process <- case defn of
          TypeOnly _ s -> Left  <$> pure (sessionKeys s)
          Paired _ p _ -> Right <$> bindM p
          TermOnly _ p -> Right <$> bindM p
    B.Call
      <$> bindM name
      <*> pure process
    -- (process, free) <- case defn of
    --       TypeOnly _ s -> return (Nothing, sessionKeys s)
    --       Paired _ p _ -> do
    --         p' <- bindM p
    --         return (Just p', B.freeChans p')
    --       TermOnly _ p -> do
    --         p' <- bindM p
    --         return (Just p', B.freeChans p')
    -- return $ B.Call (bindM name) process free loc
  bindM (Link x y loc) = buildProcess loc $
    B.Link
      <$> bindM x
      <*> bindM y
  bindM (Compose x Nothing p q loc) = buildProcess loc $
    B.Compose
      <$> bindM x
      <*> pure Nothing
      <*> bindM p
      <*> bindM q
  bindM (Compose x (Just t) p q loc) = buildProcess loc $
    B.Compose
      <$> bindM x
      <*> (Just <$> bindM t)
      <*> bindM p
      <*> bindM q
    --
    -- p' <- bindM p
    -- q' <- bindM q
    -- let free = Set.delete (chanName x) $ Set.union (B.freeChans p') (B.freeChans q')
    -- return $ B.Compose
    --           (bindM x)
    --           (fmap bindM t)
    --           p'
    --           q'
    --           free
    --           loc
  bindM (Output x y q p loc) = buildProcess loc $
    B.Output
      <$> bindM x
      <*> bindM y
      <*> bindM q
      <*> bindM p
  bindM (Input x y p loc) = buildProcess loc $
    B.Input
      <$> bindM x
      <*> bindM y
      <*> bindM p
  bindM (SelectL x p loc) = buildProcess loc $
    B.SelectL
      <$> bindM x
      <*> bindM p
  bindM (SelectR x p loc) = buildProcess loc $
    B.SelectR
      <$> bindM x
      <*> bindM p
  bindM (Choice x p q loc) = buildProcess loc $
    B.Choice
      <$> bindM x
      <*> bindM p
      <*> bindM q
  bindM (Accept x y p loc) = buildProcess loc $
    B.Accept
      <$> bindM x
      <*> bindM y
      <*> bindM p
  bindM (Request x y p loc) = buildProcess loc $
    B.Request
      <$> bindM x
      <*> bindM y
      <*> bindM p
  bindM (OutputT x t p loc) = buildProcess loc $
    B.OutputT
      <$> bindM x
      <*> bindM t
      <*> bindM p
  bindM (InputT x (TypeVar n l) p loc) = buildProcess loc $
    B.InputT
      <$> bindM x
      <*> pure (B.TypeVar n l)
      <*> bindM p
  bindM (EmptyOutput x loc) = buildProcess loc $
    B.EmptyOutput
      <$> bindM x
  bindM (EmptyInput x p loc) = buildProcess loc $
    B.EmptyInput
      <$> bindM x
      <*> bindM p
  bindM (EmptyChoice x loc) = buildProcess loc $
    B.EmptyChoice
      <$> bindM x
  bindM (End loc) = buildProcess loc (return B.End)
  bindM (Mix p q loc) = buildProcess loc $
    B.Mix
      <$> bindM p
      <*> bindM q

--------------------------------------------------------------------------------

instance Bind Program B.Program where
  bindM (Program declarations loc) =
    B.Program
      <$> bindM (toDefinitions (Program declarations loc))
      <*> pure loc

instance Bind Definition B.Definition where
  bindM (Paired name process session) =
    B.Paired
      <$> bindM name
      <*> bindM process
      <*> bindM session
  bindM (TypeOnly name session) =
    B.TypeOnly
      <$> bindM name
      <*> bindM session
  bindM (TermOnly name process) =
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
