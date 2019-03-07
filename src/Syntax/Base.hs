module Syntax.Base where

import Data.Function (on)

--------------------------------------------------------------------------------
-- | Type Variable

data TypeVar = Pos Int | Neg Int
    deriving (Show, Ord)

instance HasDual TypeVar where
  dual (Pos i) = Neg i
  dual (Neg i) = Pos i

instance Eq TypeVar where
  (==) = (==) `on` dual

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a
