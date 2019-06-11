{-# LANGUAGE OverloadedStrings                  #-}

module Pretty.Syntax.Concrete where

import Syntax.Concrete
import Pretty.Syntax.Abstract ()

import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |

instance Pretty TypeVar where
  pretty (Named i _)      = pretty i

instance Pretty Name where
  pretty = pretty . toAbstract

instance Pretty Chan where
  pretty = pretty . toAbstract

instance Pretty TypeName where
  pretty = pretty . toAbstract

instance Pretty Process where
  pretty = pretty . toAbstract

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
  
instance Pretty Session where
  pretty = pretty . toAbstract
