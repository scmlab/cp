{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Pretty.Syntax.Binding where

import Pretty.Base
import Syntax.Base
import Syntax.Binding

import qualified Data.Map as Map

import Data.Monoid ((<>))
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |

instance Pretty TypeName where
  pretty (TypeName name _) = pretty name

instance Pretty TypeVar where
  pretty (TypeVar (Bound _) name _)   = pretty name
  pretty (TypeVar (Free i) name _)   = pretty name <> "$" <> pretty i
  -- pretty (Nameless i)   = "$" <> pretty i
  -- pretty (Named i)      = pretty i
  pretty Unknown        = "$_"
  pretty (DualOf i)     = "^" <> pretty i

instance Pretty Name where
  pretty (Name name _) = pretty name

instance Pretty Chan where
  pretty (Chan _ name _) = pretty name

instance Pretty Process where
  pretty (Call x _) = pretty x
  pretty (Link x y _) = pretty x <> " ↔ " <> pretty y
  pretty (Compose x Nothing p q _) = "ν " <> pretty x
    <> " ( " <> pretty p
    <> " | " <> pretty q
    <> " )"
  pretty (Compose x t p q _) = "ν " <> pretty x
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

instance Pretty Session where
  pretty pairs
    | Map.null pairs = "[empty session]"
    | otherwise = vsep $ map p $ Map.toList pairs
      where
        p (k, v) = pretty k <> " : " <> pretty v

instance Pretty SessionSyntax where
  pretty (SessionSyntax pairs _) = pretty pairs

instance Report Session where
  report = pretty

instance Pretty Type where
  pretty (Var i _)        = pretty i
  pretty (Dual a _)       = "^" <> pretty a
  pretty (Times a b _)    = pretty a <> " ⊗ " <> pretty b
  pretty (Par a b _)      = pretty a <> " ⅋ " <> pretty b
  pretty (Plus a b _)     = pretty a <> " ⊕ " <> pretty b
  pretty (With a b _)     = pretty a <> " & " <> pretty b
  pretty (Acc a _)        = "!" <> pretty a
  pretty (Req a _)        = "?" <> pretty a
  pretty (Exists x a Nothing _)   = "∃ " <> pretty x <> " . " <> pretty a
  pretty (Exists x a (Just (b, c)) _)   = pretty a <> " { " <> pretty b <> " / " <> pretty x <>  " } = " <> pretty c
  pretty (Forall x a _)   = "∀ " <> pretty x <> " . " <> pretty a
  pretty (One _)          = "1"
  pretty (Bot _)          = "⊥"
  pretty (Zero _)         = "0"
  pretty (Top _)          = "⊤"
