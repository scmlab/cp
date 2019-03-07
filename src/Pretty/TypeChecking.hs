{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Pretty.TypeChecking where

import TypeChecking.Inference
import Pretty.Syntax.Abstract ()
import Pretty.Syntax.Concrete ()

import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |

instance Pretty Context where
  pretty pairs = vsep $ map (\(k, v) -> pretty k <> " : " <> pretty v) $ Map.toList pairs

-- instance Pretty Session where
--   pretty (Session context others) = vsep
--     [ "⊢ "
--     , indent 2 pairs
--     , indent 2 vars
--     ]
--     where
--         pairs = undefined
--         vars = undefined
