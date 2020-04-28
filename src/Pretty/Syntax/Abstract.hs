{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Pretty.Syntax.Abstract where

import           Syntax.Abstract

import           Data.Text.Prettyprint.Doc
import qualified Data.Set                      as Set

--------------------------------------------------------------------------------
-- |
--
-- varName :: Int -> String
-- varName = map (toEnum . (+) 64) . digits . succ
--   where
--     digits = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (mod x 26, div x 26))
--

--
-- instance Pretty Tree where
--   pretty (Node chan p q) =
--     "\\" <+> pretty chan <> line
--     <> indent 2 (vsep [pretty p, pretty q])
--   pretty (Leaf p) = pretty p



instance Pretty Process where
  pretty (Atom name chans) = pretty name
    <> encloseSep lbrace rbrace comma (map pretty $ Set.toList chans)
  pretty (Link x y     ) = pretty x <+> "<->" <+> pretty y
  pretty (Compose x p q) = "\\" <+> pretty x <+> line <> indent
    2
    (vsep ["(" <+> pretty p, "|" <+> pretty q, ")"])
  pretty (Output x y p q) =
    pretty x
      <>  "["
      <>  pretty y
      <>  "] . ("
      <+> pretty p
      <+> "|"
      <+> pretty q
      <+> ")"
  pretty (Input x y p) = pretty x <> "(" <> pretty y <> ") ." <+> pretty p
  pretty (SelectL x p) = pretty x <> "[inl] ." <+> pretty p
  pretty (SelectR x p) = pretty x <> "[inr] ." <+> pretty p
  pretty (Choice x p q) =
    pretty x <+> ". case(" <+> pretty p <+> "|" <+> pretty q <+> ")"
  pretty (Accept x y p) =
    "!" <> pretty x <> "(" <> pretty y <> ") ." <+> pretty p
  pretty (Request x y p) =
    "?" <> pretty x <> "[" <> pretty y <> "] ." <+> pretty p
  pretty (OutputT x p   ) = pretty x <> "[_] ." <+> pretty p
  pretty (InputT  x p   ) = pretty x <> "(_) ." <+> pretty p
  pretty (EmptyOutput x ) = pretty x <> "[] . end"
  pretty (EmptyInput x p) = pretty x <> "() ." <+> pretty p
  pretty (EmptyChoice x ) = pretty x <> ".case()"
  pretty End              = "end"
  pretty (Mix p q)        = pretty p <+> "|" <+> pretty q
