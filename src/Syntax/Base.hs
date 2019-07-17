{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Base where

import Data.Text (Text)
import Data.Loc (Loc(..))

instance Ord Loc where
  Loc _ _ `compare` Loc _ _ = EQ
  Loc _ _ `compare` NoLoc   = GT
  NoLoc   `compare` Loc _ _ = LT
  NoLoc   `compare` NoLoc   = EQ

-- --------------------------------------------------------------------------------
-- -- | Converting to Abstract Binding Tree
--
-- class ToAbstract a b | a -> b where
--   toAbstract :: a -> b

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a


-- --------------------------------------------------------------------------------
-- -- | Free/Bound variables
--
-- data Var = Free Text
--          | Bound Int
--          deriving (Eq, Ord, Show)
