module Syntax.Base where

-- import Data.Text (Text)
-- import Data.Function (on)

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a
