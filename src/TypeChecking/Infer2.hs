module TypeChecking.Infer2 where

import Syntax.Base
import Syntax.Binding
import TypeChecking.Base
import qualified TypeChecking.Unification as U
-- import TypeChecking.Unification


-- import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer hiding (Dual)

import Data.Loc (Loc(..), locOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

infer :: Process -> TCM Session
infer process = case process of
  Call (Callee name _) _ -> do
    definitions <- ask
    case Map.lookup name definitions of
      Nothing -> error "[panic] Definition not found, this shouldn't happen at the type checking state"
      Just (Annotated _ _ t) -> return t
      Just (Unannotated _ p) -> infer p

  Link x y _ -> do

    a <- fresh

    return $ Map.fromList
      [ (x, a)
      , (y, dual a)
      ]

  Compose x _ p q _ -> do

    sessionP <- infer p
    a <- lookup x sessionP

    sessionQ <- infer q
    a' <- lookup x sessionQ

    -- sessionP & sessionQ be disjoint

    subst <- unifyOpposite a a'

    return $ Map.map subst $ Map.union
        (Map.delete x sessionP)
        (Map.delete x sessionQ)

  Output x y p q _ -> do

    x `notFreeIn` p
    sessionP <- infer p
    a <- lookup y sessionP

    y `notFreeIn` q
    sessionQ <- infer q
    b <- lookup x sessionQ

    return
      $ Map.insert x (Times a b NoLoc)
      $ Map.union
          (Map.delete y sessionP)
          (Map.delete x sessionQ)

  Input x y p _ -> do

    sessionP <- infer p
    b <- lookup x sessionP
    a <- lookup y sessionP

    return
      $ Map.insert x (Par a b NoLoc)
      $ Map.delete x
      $ Map.delete y
      $ sessionP

  SelectL x p _ -> do

    sessionP <- infer p
    a <- lookup x sessionP

    b <- fresh

    return
      $ Map.insert x (Plus a b NoLoc)
      $ Map.delete x
      $ sessionP

  SelectR x p _ -> do

    sessionP <- infer p
    b <- lookup x sessionP

    a <- fresh

    return
      $ Map.insert x (Plus a b NoLoc)
      $ Map.delete x
      $ sessionP

  Choice x p q _ -> do

    sessionP <- infer p
    a <- lookup x sessionP

    sessionQ <- infer q
    b <- lookup x sessionQ

    -- sessionP sessionQ should be the same

    return
      $ Map.insert x (With a b NoLoc)
      $ Map.delete x
      $ sessionP

  Accept x y p _ -> do

    x `notFreeIn` p
    sessionP <- infer p
    a <- lookup y sessionP

    -- sessionShouldAllBeRequesting sessionP


    return
      $ Map.insert x (Acc a NoLoc)
      $ Map.delete y
      $ sessionP

  Request x y p _ -> do

    x `notFreeIn` p
    sessionP <- infer p
    a <- lookup y sessionP

    return
      $ Map.insert x (Req a NoLoc)
      $ Map.delete y
      $ sessionP

  OutputT x outputType p _ -> do

    session <- infer p
    afterSubstitution <- lookup x session  -- B {A / X}
    beforeSubstitution <- fresh   -- B

    let t = (Exists
              Unknown                     -- the type variable to be substituted
              beforeSubstitution          -- the type before substitution
              (Just
                ( outputType   -- the type to be substituted with
                , afterSubstitution       -- the resulting type after substitution
                ))
              NoLoc)

    return
      $ Map.insert x t
      $ Map.delete x
      $ session


  InputT x var p _ -> do

    sessionP <- infer p
    b <- lookup x sessionP

    return
      $ Map.insert x (Forall var b NoLoc)
      $ Map.delete x
      $ sessionP

  EmptyInput x p _ -> do

    x `notFreeIn` p
    sessionP <- infer p

    return
      $ Map.insert x (Bot NoLoc)
      $ sessionP

  EmptyOutput x _ -> do

    return $ Map.fromList
      [ (x, One NoLoc)
      ]

  End _ -> return Map.empty

  others -> error $ show others

  where
    -- If a channel is free in some process, we should be able to get its
    --  type from the inferred session
    -- However, if we are asking for something that doesn't exist
    --  we can still apply the Weakening rule and return something wrapped in "?"
    lookup :: Chan -> Session -> TCM Type
    lookup chan session = case Map.lookup chan session of
      Nothing -> Req <$> fresh <*> pure NoLoc -- Weakening!!
      Just t -> return t

    -- assert that some channel shouldn't occur free in some process
    notFreeIn :: Chan -> Process -> TCM ()
    notFreeIn channel term = do
      let Chan _ name _ = channel
      when (name `Set.member` freeVariables term) $
        throwError $ ChanFound term channel

    -- return a fresh type variable
    fresh :: TCM Type
    fresh = do
      i <- get
      put (succ i)
      return $ Var (TypeVar (Bound i) "_" NoLoc) NoLoc

    -- unify the two given types, and return a substitution function
    -- for better error message, make the former type be the expecting type
    -- and the latter be the given type
    unify :: Type -> Type -> TCM (Type -> Type)
    unify expected given = do
      let (result, subst) = U.unify expected given
      case result of
        Left (t, u) -> throwError $ TypeMismatch process expected given t u
        Right _ -> return $ compose $ map U.apply subst
          where
            -- compose a list of functions
            compose = foldl (flip (.)) id

    -- taking extra care when unifying two opposite types
    -- because we might will lose something when taking the dual of (Exists _ _ _)
    unifyOpposite :: Type -> Type -> TCM (Type -> Type)
    unifyOpposite a@(Exists _ _ _ _) b@(Forall _ _ _) = unify a (dual b)
    unifyOpposite a@(Forall _ _ _) b@(Exists _ _ _ _) = unify (dual a) b
    unifyOpposite a b                                 = unify a (dual b)


--------------------------------------------------------------------------------
-- | TCM

inferProcess :: Process -> TCM Session
inferProcess = infer
