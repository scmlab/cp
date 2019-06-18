module Syntax.Base where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a


--------------------------------------------------------------------------------
-- | Free/Bound variables

data Var = Free Text
         | Bound Int
         deriving (Eq, Ord, Show)
