{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Base where

import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Loc (Loc(..))

import Control.Monad.State

instance Ord Loc where
  Loc _ _ `compare` Loc _ _ = EQ
  Loc _ _ `compare` NoLoc   = GT
  NoLoc   `compare` Loc _ _ = LT
  NoLoc   `compare` NoLoc   = EQ

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

type BindingM = State BindingState

class ToBinding a b | a -> b where
  toBindingM :: a -> BindingM b

toBinding :: ToBinding a b => a -> b
toBinding x =
  evalState
    (toBindingM x)
    (BindingState (Binding 0 Set.empty) (Binding 0 Set.empty))

-- --------------------------------------------------------------------------------
-- -- | Converting to Abstract Binding Tree
--
-- class ToAbstract a b | a -> b where
--   toAbstract :: a -> b

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a


--------------------------------------------------------------------------------
-- | Free/Bound variables

data Var = Free Text
         | Bound Int
         deriving (Eq, Ord, Show)
