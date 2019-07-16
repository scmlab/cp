{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Pretty.Syntax.Binding where

import Pretty.Base
import Syntax.Base
import Syntax.Binding

import qualified Data.Map as Map

import Data.Monoid ((<>))
import Data.List (unfoldr)
import Data.Text.Prettyprint.Doc hiding (line)

--------------------------------------------------------------------------------
-- |

instance Pretty TypeName where
  pretty (TypeName name _) = pretty name

varName :: Int -> String
varName = map (toEnum . (+) 64) . digits . succ
  where
    digits = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (mod x 26, div x 26))


instance Pretty TypeVar where
  pretty (TypeVar (Bound i) "_" _) = "$" <> pretty (varName i)
  pretty (TypeVar (Bound i) name _)   = pretty name <> "$" <> pretty (varName i)
  pretty (TypeVar (Free _) name _)   = pretty name
  pretty Unknown        = "$_"
  -- pretty (DualOf i)     = "^" <> pretty i

instance Pretty Name where
  pretty (Name name _) = pretty name

instance Pretty Chan where
  pretty (Chan (Bound i) name _) = pretty name <> "$" <> pretty i
  pretty (Chan (Free _) name _) = pretty name

instance Pretty Process where
  pretty (Call name _ _) = pretty name
  -- pretty (Call name Nothing _) = pretty name
  -- pretty (Call _ (Just p) _) = pretty p
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

instance PrettyPrec Type where
  -- nullary
  prettyPrec _ (Var i _)      = pretty i
  prettyPrec _ (One _)        = "1"
  prettyPrec _ (Bot _)        = "⊥"
  prettyPrec _ (Zero _)       = "0"
  prettyPrec _ (Top _)        = "⊤"
  -- unary, with the highest precedences
  prettyPrec p (Dual a _)     = parensIf (p > 19) $
    "^" <> prettyPrec 19 a
  prettyPrec p (Acc a _)      = parensIf (p > 18) $
    "!" <+> prettyPrec 18 a
  prettyPrec p (Req a _)      = parensIf (p > 17) $
    "?" <+> prettyPrec 17 a
  -- binary, with lower precedences
  prettyPrec p (Times a b _)  = parensIf (p > 8) $
    prettyPrec 9 a <+> "⊗" <+> prettyPrec 8 b
  prettyPrec p (Par a b _)    = parensIf (p > 7) $
    prettyPrec 8 a <+> "⅋" <+> prettyPrec 7 b
  prettyPrec p (Plus a b _)    = parensIf (p > 6) $
    prettyPrec 7 a <+> "⊕" <+> prettyPrec 6 b
  prettyPrec p (With a b _)    = parensIf (p > 5) $
    prettyPrec 6 a <+> "&" <+> prettyPrec 5 b
  prettyPrec p (Exists x a Nothing _) = parensIf (p > 4) $
    "∃" <+> pretty x <+> "." <+> prettyPrec 4 a
  prettyPrec p (Exists x a (Just (b, c)) _) = parensIf (p > 4) $
    prettyPrec 4 a
      <+> "{"
      <+> prettyPrec 4 b
      <+> "/"
      <+> pretty x
      <+> "} ="
      <+> prettyPrec 4 c
  prettyPrec p (Forall x a _)   = parensIf (p > 3) $
    "∀" <+> pretty x <+> "." <+> prettyPrec 3 a

instance Pretty Type where
  pretty = prettyPrec 0
