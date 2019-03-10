module Syntax.Base where

import Data.Text (Text)
import Data.Function (on)

--------------------------------------------------------------------------------
-- | Type Variable

data TypeVar = Nameless Int | Named Text | Unknown
    deriving (Show, Ord, Eq)

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a
