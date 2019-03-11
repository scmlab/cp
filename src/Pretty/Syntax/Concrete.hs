{-# LANGUAGE OverloadedStrings                  #-}

module Pretty.Syntax.Concrete where

import Syntax.Concrete

import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |

instance Pretty (TypeVar ann) where
  pretty (Named i _)      = pretty i

instance Pretty (TermName ann) where
  pretty (TermName name _) = pretty name

instance Pretty (TypeName ann) where
  pretty (TypeName name _) = pretty name

instance Pretty (Process ann) where
  pretty (Call x _) = pretty x
  pretty (Link x y _) = pretty x <> " ↔ " <> pretty y
  pretty (Compose x t p q _) = pretty x
    <> " : " <> pretty t
    <> " ( " <> pretty p
    <> " | " <> pretty q
    <> " )"
  pretty (Output x y p q _) = pretty x
    <> "[" <> pretty y <> "] . ( "
    <> pretty p <> " | " <> pretty q <> " )"
  pretty (Input x y p _) = pretty x
    <> "(" <> pretty y <> ") . "
    <> pretty p
  pretty (SelectL x p _) = pretty x
    <> "[inl] . "
    <> pretty p
  pretty (SelectR x p _) = pretty x
    <> "[inr] . "
    <> pretty p
  pretty (Choice x p q _) = pretty x
    <> ". case( " <> pretty p <> " | " <> pretty q <> " )"
  pretty (Accept x y p _) = "!"
    <> pretty x
    <> "(" <> pretty y <> ") . "
    <> pretty p
  pretty (Request x y p _) = "?"
    <> pretty x
    <> "[" <> pretty y <> "] . "
    <> pretty p
  pretty (OutputT x y p _) = pretty x
    <> "[" <> pretty y <> "] . "
    <> pretty p
  pretty (InputT x y p _) = pretty x
    <> "(" <> pretty y <> ") . "
    <> pretty p
  pretty (EmptyOutput x _) = pretty x
    <> "[] . end"
  pretty (EmptyInput x p _) = pretty x
    <> "() . " <> pretty p
  pretty (EmptyChoice x _) = pretty x
    <> ".case()"
  pretty (End _) = "end"
  pretty (Mix p q _) = pretty p <> " | " <> pretty q

instance Pretty (Type ann) where
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
