module Syntax.Base where

import Data.Function (on)

--------------------------------------------------------------------------------
-- | Type Variable

data TypeVar = Pos Int | Neg Int | Unknown
    deriving (Show, Ord)

instance HasDual TypeVar where
  dual (Pos i) = Neg i
  dual (Neg i) = Pos i
  dual Unknown = Unknown

instance Eq TypeVar where
  x == y = case (dual x, dual y) of
    (Unknown, Unknown) -> True
    (Pos i , Pos j) -> i == j
    (Neg i , Neg j) -> i == j
    _ -> False

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a
