{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module TypeChecking.Binding where

import Syntax.Base
import Syntax.Concrete
import qualified Syntax.Binding as B
import TypeChecking.Base

import Data.Loc (Loc(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (LT, EQ, GT)

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Debug.Trace

--------------------------------------------------------------------------------
-- | Converting to Concrete Binding Tree

data Binding = Binding
  { bindingBound :: Int
  , bindingFree :: Set Text
  } deriving (Show)

data BindingState = BindingState
  { bsChannel :: Binding
  , bsTypeVar :: Binding
  -- should be reset when checking each definitions
  , bsTraversed :: Set Name
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
    initialBindingState =
      BindingState
        (Binding 0 Set.empty)
        (Binding 0 Set.empty)
        Set.empty

askDefn :: Name -> Loc -> BindM Definition
askDefn name loc = do
  result <- Map.lookup name <$> ask
  case result of
    Nothing -> throwError $ DefnNotFound (Call name loc) name
    Just definition -> return definition


resetTraversed :: BindM ()
resetTraversed = do
  modify $ \ st -> st { bsTraversed = Set.empty }


markTraversed :: Name -> BindM ()
markTraversed name = do
  modify $ \ st -> st { bsTraversed = Set.insert name (bsTraversed st) }

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

instance Bind Process B.Process where
  bindM (Call name loc) = do
    -- see if name has been traversed
    traversed <- Set.member name <$> gets bsTraversed
    callee <- if traversed
      then throwError $ RecursiveCall (Call name loc) name
      else do
        process <- toProcess <$> askDefn name loc
        -- mark `name` as traversed
        markTraversed name
        B.Callee
          <$> bindM name
          <*> bindM process
        -- return callee

    -- callee <- case result of
    --   -- register a new calee
    --   Nothing -> do
    --     traceShow ("no " ++ show name) (return ())
    --     process <- toProcess <$> askDefn name loc
    --     -- insert a placeholder to prevent `bindM` from looping forever
    --     modify $ \ st -> st { bsCallees = Map.insert name Nothing (bsCallees st) }
    --     -- trying to do the real work (with perhaps some placeholders)
    --     callee <- B.Callee
    --                 <$> bindM name
    --                 <*> (Just <$> bindM process)
    --     traceShow (show callee) (return ())
    --
    --     -- update the placeholder
    --     modify $ \ st -> st { bsCallees = Map.insert name (Just callee) (bsCallees st) }
    --     return callee
    --   Just (Just c) -> return c
    --   Just Nothing -> do
    --     traceShow ("some " ++ show name) (return ())
    --     B.Callee <$> bindM name <*> pure Nothing
    return $ B.Call callee loc
  bindM (Link x y loc) =
    B.Link
      <$> bindM x
      <*> bindM y
      <*> pure loc
  bindM (Compose x Nothing p q loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> pure Nothing
      <*> (bind <$> bindM p)
      <*> (bind <$> bindM q)
      <*> pure loc
  bindM (Compose x (Just t) p q loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> (Just <$> bindM t)
      <*> (bind <$> bindM p)
      <*> (bind <$> bindM q)
      <*> pure loc
  bindM (Output x y q p loc) = do
    (varB, bindB) <- createBinderChan y
    B.Output
      <$> bindM x
      <*> pure varB
      <*> (bindB <$> bindM q)
      <*> bindM p
      <*> pure loc
  bindM (Input x y proc loc) = do
    (varB, bindB) <- createBinderChan y
    B.Input
      <$> bindM x
      <*> pure varB
      <*> (bindB <$> bindM proc)
      <*> pure loc
  bindM (SelectL x proc loc) =
    B.SelectL
      <$> bindM x
      <*> bindM proc
      <*> pure loc
  bindM (SelectR x proc loc) =
    B.SelectR
      <$> bindM x
      <*> bindM proc
      <*> pure loc
  bindM (Choice x p q loc) =
    B.Choice
      <$> bindM x
      <*> bindM p
      <*> bindM q
      <*> pure loc
  bindM (Accept x y p loc) = do
    (varB, bindB) <- createBinderChan y
    B.Accept
      <$> bindM x
      <*> pure varB
      <*> (bindB <$> bindM p)
      <*> pure loc
  bindM (Request x y p loc) = do
    (varB, bindB) <- createBinderChan y
    B.Request
      <$> bindM x
      <*> pure varB
      <*> (bindB <$> bindM p)
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
      <*> pure (B.TypeVar (Free n) n l)
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
  bindM (Annotated name process session) = do
    resetTraversed
    B.Annotated
      <$> bindM name
      <*> bindM process
      <*> bindM session
  bindM (Unannotated name process) = do
    resetTraversed
    B.Unannotated
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
