{-# LANGUAGE OverloadedStrings                  #-}

module Pretty.Syntax.Concrete where

import Syntax.Base
import Syntax.Concrete
import Pretty.Syntax.Binding ()

import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |

instance Pretty TypeVar where
  pretty (TypeVar i _)      = pretty i

instance Pretty Name where
  pretty = pretty . toBinding

instance Pretty Chan where
  pretty = pretty . toBinding

instance Pretty TypeName where
  pretty = pretty . toBinding

instance Pretty Process where
  pretty = pretty . toBinding

instance Pretty Type where
  pretty (Var i _)        = "$" <> pretty i
  pretty (Dual a _)       = "^" <> pretty a
  pretty (Times a b _)    = pretty a <> " ⊗ " <> pretty b
  pretty (Par a b _)      = pretty a <> " ⅋ " <> pretty b
  pretty (Plus a b _)     = pretty a <> " ⊕ " <> pretty b
  pretty (With a b _)     = pretty a <> " & " <> pretty b
  pretty (Acc a _)        = "!" <> pretty a
  pretty (Req a _)        = "?" <> pretty a
  pretty (Exists x a _)   = "∃ " <> pretty x <> " . " <> pretty a
  pretty (Forall x a _)   = "∀ " <> pretty x <> " . " <> pretty a
  pretty (One _)          = "𝟙"
  pretty (Bot _)          = "⊥"
  pretty (Zero _)         = "𝟘"
  pretty (Top _)          = "⊤"

instance Pretty SessionSyntax where
  pretty = pretty . toBinding
