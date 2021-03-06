{-# LANGUAGE OverloadedStrings #-}

module TypeChecking.Infer where

import           Syntax.Base
import           Syntax.Concrete
import           TypeChecking.Base
import qualified TypeChecking.Unification      as U
-- import TypeChecking.Unification


-- import Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Reader
-- import Control.Monad.Writer hiding (Dual)

import           Data.Loc                       ( Loc(..) )
-- import Data.Map (Map)
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Text                      ( pack )

import           Prelude                 hiding ( lookup )

-- import           Debug.Trace
-- import           Pretty

check :: Name -> Process -> Session -> TCM ()
check _ process expected = do
  given <- infer process
  let difference = Map.union (Map.difference expected given)
                             (Map.difference given expected)

  -- see if the keys of two Maps look the same
  unless (Map.null difference) $ throwError $ SessionMismatch process
                                                              expected
                                                              given

  -- look into the types and see if they are also the same
  forM_ (Map.intersectionWith (,) expected given) (uncurry checkIfEqual)

 where
  checkIfEqual :: Type -> Type -> TCM ()
  checkIfEqual expected' given' = case U.unify expected' given' of
    Left  (t, u) -> throwError $ TypeMismatch process expected' given' t u
    Right _      -> return ()

infer :: Process -> TCM Session
infer process = case process of
  Call name _ -> do
    definitions <- ask
    case Map.lookup name definitions of
      Nothing ->
        error
          "[panic] Definition not found, this shouldn't happen at the type checking state"
      Just (Paired _ _ t) -> return t
      Just (TypeOnly _ t) -> return t
      Just (TermOnly _ p) -> infer p

  Link x y _ -> do

    a <- fresh

    return $ Map.fromList [(x, a), (y, dual a)]

  Compose x _ p q _ -> do

    sessionP <- infer p
    a        <- lookup x sessionP

    sessionQ <- infer q
    a'       <- lookup x sessionQ

    let sessionP' = Map.delete x sessionP
    let sessionQ' = Map.delete x sessionQ


    sessionP' `disjoint` sessionQ'

    subst <- unifyOpposite a a'

    return $ Map.map subst $ Map.union sessionP' sessionQ'

  Output x y p q _ -> do

    x `notFreeIn` p
    sessionP <- infer p
    a        <- lookup y sessionP

    y `notFreeIn` q
    sessionQ <- infer q
    b        <- lookup x sessionQ

    let sessionP' = Map.delete y sessionP
    let sessionQ' = Map.delete x sessionQ

    sessionP' `disjoint` sessionQ'

    return $ Map.insert x (Times a b NoLoc) $ Map.union sessionP' sessionQ'

  Input x y p _ -> do

    session <- infer p
    b       <- lookup x session
    a       <- lookup y session

    let session' = Map.delete x $ Map.delete y session

    return $ Map.insert x (Par a b NoLoc) $ session'

  SelectL x p _ -> do

    session <- infer p
    a       <- lookup x session

    let session' = Map.delete x session

    b <- fresh

    return $ Map.insert x (Plus a b NoLoc) $ session'

  SelectR x p _ -> do

    session <- infer p
    b       <- lookup x session

    let session' = Map.delete x session

    a <- fresh

    return $ Map.insert x (Plus a b NoLoc) $ session'

  Choice x p q _ -> do

    sessionP <- infer p
    a        <- lookup x sessionP

    sessionQ <- infer q
    b        <- lookup x sessionQ


    let sessionP' = Map.delete x sessionP
    let sessionQ' = Map.delete x sessionQ

    sessionP' `equal` sessionQ'

    return $ Map.insert x (With a b NoLoc) $ sessionP'

  Accept x y p _ -> do

    x `notFreeIn` p
    session <- infer p
    a       <- lookup y session

    let session' = Map.delete y session

    allRequest session'

    return $ Map.insert x (Acc a NoLoc) $ session'

  Request x y p _ -> do

    session <- infer p

    a       <- lookup y session

    -- it's okay for `x` to be free in `p`
    -- assume that if `x : B` and `y : A`
    -- then `B ~ ?A`
    b       <- lookup x session

    subst   <- unify (Req a NoLoc) b

    let session' = Map.delete y session

    return $ Map.map subst session'

  OutputT x outputType p _ -> do

    session            <- infer p
    afterSubstitution  <- lookup x session  -- B {A / X}
    beforeSubstitution <- fresh   -- B

    let t =
          (Exists
            Unknown                     -- the type variable to be substituted
            beforeSubstitution          -- the type before substitution
            (Just
              ( outputType              -- the type to be substituted with
              , afterSubstitution       -- the resulting type after substitution
              )
            )
            NoLoc
          )
    let session' = Map.delete x session

    return $ Map.insert x t $ session'

  InputT x var p _ -> do

    session <- infer p
    b       <- lookup x session
    let session' = Map.delete x session

    return $ Map.insert x (Forall var b NoLoc) $ session'

  EmptyInput x p _ -> do

    x `notFreeIn` p
    session <- infer p

    return $ Map.insert x (Bot NoLoc) $ session

  EmptyOutput x _ -> return $ Map.singleton x (One NoLoc)

  EmptyChoice x _ -> return $ Map.singleton x (Top NoLoc)

  End _           -> return Map.empty

  Mix p q _       -> do

    sessionP <- infer p
    sessionQ <- infer q

    sessionP `disjoint` sessionQ

    return $ Map.union sessionP sessionQ


 where
    -- If a channel is free in some process, we should be able to get its
    --  type from the inferred session
    -- However, if we are asking for something that doesn't exist
    --  we can still apply the Weakening rule and return something wrapped in "?"
  lookup :: Chan -> Session -> TCM Type
  lookup chan session = case Map.lookup chan session of
    Nothing -> do
      t <- fresh
      return $ Req t NoLoc -- Weakening!!
    Just t -> return t

  -- assert that some channel shouldn't occur free in some process
  notFreeIn :: Chan -> Process -> TCM ()
  notFreeIn channel process' = do
    free <- freeChans process'
    when (channel `Set.member` free) $ throwError $ ChannelNotComsumed
      process'
      channel

  -- return a fresh type variable
  fresh :: TCM Type
  fresh = do
    i <- get
    put (succ i)
    let name = "#" <> pack (show i)
    return $ Var (TypeVar name NoLoc) NoLoc

  -- unify the two given types, and return a substitution function
  -- for better error message, make the former type be the expecting type
  -- and the latter be the given type
  unify :: Type -> Type -> TCM (Type -> Type)
  unify expected given = case U.unify expected given of
    Left  (t, u) -> throwError $ TypeMismatch process expected given t u
    Right subst  -> return subst

  -- taking extra care when unifying two opposite types
  -- because we might will lose something when taking the dual of (Exists _ _ _)
  unifyOpposite :: Type -> Type -> TCM (Type -> Type)
  unifyOpposite a@(Exists _ _ _ _) b@(Forall _ _ _  ) = unify a (dual b)
  unifyOpposite a@(Forall _ _ _  ) b@(Exists _ _ _ _) = unify (dual a) b
  unifyOpposite a                  b                  = unify a (dual b)

  -- assert that two sessions be disjoint
  disjoint :: Session -> Session -> TCM ()
  disjoint a b =
    unless (Set.disjoint (Map.keysSet a) (Map.keysSet b))
      $ throwError
      $ SessionNotDisjoint process a b

  -- assert that two sessions be equal
  equal :: Session -> Session -> TCM ()
  equal a b = unless (a == b) $ throwError $ SessionMismatch process a b

  allRequest :: Session -> TCM ()
  allRequest session =
    unless (Map.null outliers) $ throwError $ SessionNotAllRequest process
                                                                   outliers
   where
    outliers :: Session
    outliers = Map.filter (not . isRequesting) session

    isRequesting :: Type -> Bool
    isRequesting (Req _ _) = True
    isRequesting _         = False


--------------------------------------------------------------------------------
-- | TCM

inferProcess :: Process -> TCM Session
inferProcess = infer

freeChans :: Process -> TCM (Set Chan)
freeChans process = case process of
  Call name _ -> do
    definitions <- ask
    case Map.lookup name definitions of
      Nothing             -> return Set.empty
      Just (Paired _ p _) -> freeChans p
      Just (TypeOnly _ t) -> return $ Map.keysSet t
      Just (TermOnly _ p) -> freeChans p
  Link x y _ -> return $ Set.fromList [x, y]
  Compose x _ p q _ ->
    Set.delete x <$> (Set.union <$> freeChans p <*> freeChans q)
  Output x y p q _ ->
    Set.insert x . Set.delete y <$> (Set.union <$> freeChans p <*> freeChans q)
  Input x y p _ -> Set.insert x . Set.delete y <$> freeChans p
  SelectL x p _ -> Set.insert <$> pure x <*> freeChans p
  SelectR x p _ -> Set.insert <$> pure x <*> freeChans p
  Choice x p q _ ->
    Set.insert x <$> (Set.union <$> freeChans p <*> freeChans q)
  Accept  x y p _  -> Set.insert x . Set.delete y <$> freeChans p
  Request x y p _  -> Set.insert x . Set.delete y <$> freeChans p
  OutputT x _ p _  -> Set.insert <$> pure x <*> freeChans p
  InputT  x _ p _  -> Set.insert <$> pure x <*> freeChans p
  EmptyOutput x _  -> Set.singleton <$> pure x
  EmptyInput x p _ -> Set.insert <$> pure x <*> freeChans p
  EmptyChoice x _  -> Set.singleton <$> pure x
  End _            -> return Set.empty
  Mix p q _        -> Set.union <$> freeChans p <*> freeChans q
