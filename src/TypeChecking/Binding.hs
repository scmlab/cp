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

class Build a b | a -> b where
  build :: a -> b

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

instance Build TypeVar B.TypeVar where
  build (TypeVar name loc) = B.TypeVar name loc

instance Build Type B.Type where
  build (Var i loc) = B.Var (build i) loc
  build (Dual t loc) = B.Dual (build t) loc
  build (Times t u loc) = B.Times (build t) (build u) loc
  build (Par t u loc) = B.Par (build t) (build u) loc
  build (Plus t u loc) = B.Plus (build t) (build u) loc
  build (With t u loc) = B.With (build t) (build u) loc
  build (Acc t loc) = B.Acc (build t) loc
  build (Req t loc) = B.Req (build t) loc
  build (Exists x t loc) = B.Exists (build x) (build t) Nothing loc
  build (Forall x t loc) = B.Forall (build x) (build t) loc
  build (One loc) = B.One loc
  build (Bot loc) = B.Bot loc
  build (Zero loc) = B.Zero loc
  build (Top loc) = B.Top loc

--------------------------------------------------------------------------------

instance Build Name B.Name where
  build (Name name loc) = B.Name name loc

instance Build TypeName B.TypeName where
  build (TypeName name loc) = B.TypeName name loc

instance Build Chan B.Chan where
  build (Chan name loc) = B.Chan name loc

buildM :: Build a b => a -> BindM b
buildM = return . build

instance Bind Process B.Process where
  bindM (Call name loc) = do
    defn <- askDefn name loc
    (process, free) <- case defn of
          TypeOnly _ s -> return (Nothing, sessionKeys s)
          Paired _ p _ -> do
            p' <- bindM p
            return (Just p', B.freeChans p')
          TermOnly _ p -> do
            p' <- bindM p
            return (Just p', B.freeChans p')
    return $ B.Call (build name) process free loc
  bindM (Link x y loc) = return $
    B.Link
      (build x)
      (build y)
      (Set.fromList [chanName x, chanName y])
      loc
  bindM (Compose x t p q loc) = do
    p' <- bindM p
    q' <- bindM q
    let free = Set.delete (chanName x) $ Set.union (B.freeChans p') (B.freeChans q')
    return $ B.Compose
              (build x)
              (fmap build t)
              p'
              q'
              free
              loc
  bindM (Output x y q p loc) = do
    B.Output
      <$> buildM x
      <*> buildM y
      <*> bindM q
      <*> bindM p
      <*> pure loc
  bindM (Input x y p loc) = do
    B.Input
      <$> buildM x
      <*> buildM y
      <*> bindM p
      <*> pure loc
  bindM (SelectL x p loc) =
    B.SelectL
      <$> buildM x
      <*> bindM p
      <*> pure loc
  bindM (SelectR x p loc) =
    B.SelectR
      <$> buildM x
      <*> bindM p
      <*> pure loc
  bindM (Choice x p q loc) =
    B.Choice
      <$> buildM x
      <*> bindM p
      <*> bindM q
      <*> pure loc
  bindM (Accept x y p loc) = do
    B.Accept
      <$> buildM x
      <*> buildM y
      <*> bindM p
      <*> pure loc
  bindM (Request x y p loc) = do
    B.Request
      <$> buildM x
      <*> buildM y
      <*> bindM p
      <*> pure loc
  bindM (OutputT x t p loc) =
    B.OutputT
      <$> buildM x
      <*> buildM t
      <*> bindM p
      <*> pure loc
  bindM (InputT x (TypeVar n l) p loc) = do
    B.InputT
      <$> buildM x
      <*> pure (B.TypeVar n l)
      <*> bindM p
      <*> pure loc
  bindM (EmptyOutput x loc) =
    B.EmptyOutput
      <$> buildM x
      <*> pure loc
  bindM (EmptyInput x p loc) =
    B.EmptyInput
      <$> buildM x
      <*> bindM p
      <*> pure loc
  bindM (EmptyChoice x loc) = return $
    B.EmptyChoice (build x) loc
  bindM (End loc) = return $ B.End loc
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

instance Bind Definition B.Definition where
  bindM (Paired name process session) = do
    process' <- bindM process
    return $ B.Paired (build name) process' (build session)
  bindM (TypeOnly name session) = do
    return $ B.TypeOnly (build name) (build session)
  bindM (TermOnly name process) = do
    process' <- bindM process
    return $ B.TermOnly (build name) process'

instance Bind Definitions B.Definitions where
  bindM pairs = do
    let (keys, elems) = unzip $ Map.toList pairs
    let keys' = map build keys
    elems' <- mapM bindM elems
    return $ Map.fromList (zip keys' elems')

instance Build Session B.Session where
  build pairs = Map.fromList (zip keys' elems')
    where
      (keys, elems) = unzip $ Map.toList pairs
      keys' = map build keys
      elems' = map build elems

instance Build SessionSyntax B.SessionSyntax where
  build (SessionSyntax pairs loc) = B.SessionSyntax (build pairs) loc
