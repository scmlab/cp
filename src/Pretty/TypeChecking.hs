{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Pretty.TypeChecking where

import TypeChecking.Inference
import Pretty.Syntax.Abstract ()
import Pretty.Syntax.Concrete ()

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc

--------------------------------------------------------------------------------
-- |

instance Pretty Session where
  pretty pairs = vsep $ map p $ Map.toList pairs
    where
      p (k, v) = pretty k <> " : " <> pretty v
      -- p (k, (Aligned, v)) = pretty k <> " : " <> pretty v
      -- p (k, (Undecided ctxs, v)) = pretty k <> " : " <> pretty v <> " of " <> pretty (Set.toList ctxs)
