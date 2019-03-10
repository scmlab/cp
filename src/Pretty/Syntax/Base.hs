{-# LANGUAGE OverloadedStrings                  #-}

module Pretty.Syntax.Base where

import Syntax.Base

import Data.Monoid (mempty, (<>))
import Data.Text.Prettyprint.Doc hiding (line)
import Data.Text.Prettyprint.Doc.Render.Terminal

--------------------------------------------------------------------------------
-- |

instance Pretty TypeVar where
  pretty (Pos i)        = pretty i
  pretty (Neg i)        = "^" <> pretty i
  pretty Unknown        = "$_"
