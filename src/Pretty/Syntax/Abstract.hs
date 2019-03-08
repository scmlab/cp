{-# LANGUAGE OverloadedStrings                  #-}

module Pretty.Syntax.Abstract where

import Syntax.Abstract
import Pretty.Syntax.Base ()

import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |

instance Pretty Process where
  pretty (Link x y) = pretty x <> " ↔ " <> pretty y
  pretty (Compose x t p q) = pretty x
    <> " : " <> pretty t
    <> " ( " <> pretty p
    <> " | " <> pretty q
    <> " )"
  pretty (Output x y p q) = pretty x
    <> "[" <> pretty y <> "] . ( "
    <> pretty p <> " | " <> pretty q <> " )"
  pretty (Input x y p) = pretty x
    <> "(" <> pretty y <> ") . "
    <> pretty p
  pretty (SelectL x p) = pretty x
    <> "[inl] . "
    <> pretty p
  pretty (SelectR x p) = pretty x
    <> "[inr] . "
    <> pretty p
  pretty (Choice x p q) = pretty x
    <> ". case( " <> pretty p <> " | " <> pretty q <> " )"
  pretty (Accept x y p) = "!"
    <> pretty x
    <> "(" <> pretty y <> ") . "
    <> pretty p
  pretty (Request x y p) = "?"
    <> pretty x
    <> "[" <> pretty y <> "] . "
    <> pretty p
  pretty (OutputT x y p) = pretty x
    <> "[" <> pretty y <> "] . "
    <> pretty p
  pretty (InputT x y p) = pretty x
    <> "(" <> pretty y <> ") . "
    <> pretty p
  pretty (EmptyOutput x) = pretty x
    <> "[] . end"
  pretty (EmptyInput x p) = pretty x
    <> "() . " <> pretty p
  pretty (EmptyChoice x) = pretty x
    <> ".case()"

instance Pretty Type where
  pretty (Var i)        = "$" <> pretty i
  pretty (Dual a)       = "^" <> pretty a
  pretty (Times a b)    = pretty a <> "⊗" <> pretty b
  pretty (Par a b)      = pretty a <> "⅋" <> pretty b
  pretty (Plus a b)     = pretty a <> "⊕" <> pretty b
  pretty (With a b)     = pretty a <> "&" <> pretty b
  pretty (Acc a)        = "!" <> pretty a
  pretty (Req a)        = "?" <> pretty a
  pretty (Exists x a)   = "∃ " <> pretty x <> " . " <> pretty a
  pretty (Forall x a)   = "∀ " <> pretty x <> " . " <> pretty a
  pretty (One)          = "𝟙"
  pretty (Bot)          = "⊥"
  pretty (Zero)         = "𝟘"
  pretty (Top)          = "⊤"