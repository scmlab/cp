{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Concrete where

import Data.Text (Text)
import Data.Loc
import Prelude hiding (LT, EQ, GT)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data TermName     ann = TermName Text ann
                      deriving (Show, Functor)
data TypeName     ann = TypeName Text ann
                      deriving (Show, Functor)

data Program      ann = Program     [Declaration ann]                       ann
                      deriving (Show, Functor)

data Declaration  ann = TypeDecl  (TypeName ann)  (Type ann)                ann
                      | TermDecl  (TermName ann)  (Process ann)             ann
                      deriving (Show, Functor)

data Process  ann = Link      (TermName ann) (TermName ann)                 ann
                  | Compose   (TermName ann) (Process  ann) (Process ann)   ann
                  | Output    (TermName ann) (TermName ann) (Process ann) (Process ann) ann
                  | Input     (TermName ann) (TermName ann) (Process ann)   ann
                  | SelectL   (TermName ann) (Process  ann)                 ann
                  | SelectR   (TermName ann) (Process  ann)                 ann
                  | Choice    (TermName ann) (Process  ann) (Process ann)   ann
                  | Accept    (TermName ann) (TermName ann) (Process ann)   ann
                  | Request   (TermName ann) (TermName ann) (Process ann)   ann
                  | EmptyOutput              (TermName ann)                 ann
                  | EmptyInput               (TermName ann) (Process ann)   ann
                  | EmptyChoice              (TermName ann)                 ann
                  deriving (Show, Functor)

data Type ann = Dual    (Type ann)                  ann
              | Times   (Type ann)      (Type ann)  ann
              | Par     (Type ann)      (Type ann)  ann
              | Plus    (Type ann)      (Type ann)  ann
              | With    (Type ann)      (Type ann)  ann
              | Acc     (Type ann)                  ann
              | Req     (Type ann)                  ann
              | Exists  (TypeName ann)  (Type ann)  ann
              | Forall  (TypeName ann)  (Type ann)  ann
              | One                                 ann
              | Bot                                 ann
              | Zero                                ann
              | Top                                 ann
              deriving (Show, Functor)

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located (TypeName Loc) where
  locOf (TypeName _ loc) = loc

instance Located (TermName Loc) where
  locOf (TermName _ loc) = loc

instance Located (Program Loc) where
  locOf (Program _ loc) = loc

instance Located (Declaration Loc) where
  locOf (TypeDecl _ _ loc) = loc
  locOf (TermDecl _ _ loc) = loc

instance Located (Process Loc) where
  locOf (Link _ _ loc) = loc
  locOf (Compose _ _ _ loc) = loc
  locOf (Output _ _ _ _ loc) = loc
  locOf (Input _ _ _ loc) = loc
  locOf (SelectL _ _ loc) = loc
  locOf (SelectR _ _ loc) = loc
  locOf (Choice _ _ _ loc) = loc
  locOf (Accept _ _ _ loc) = loc
  locOf (Request _ _ _ loc) = loc
  locOf (EmptyOutput _ loc) = loc
  locOf (EmptyInput _ _ loc) = loc
  locOf (EmptyChoice _ loc) = loc

instance Located (Type Loc) where
  locOf (Dual _ loc) = loc
  locOf (Times _ _ loc) = loc
  locOf (Par _ _ loc) = loc
  locOf (Plus _ _ loc) = loc
  locOf (With _ _ loc) = loc
  locOf (Acc _ loc) = loc
  locOf (Req _ loc) = loc
  locOf (Exists _ _ loc) = loc
  locOf (Forall _ _ loc) = loc
  locOf (One loc) = loc
  locOf (Bot loc) = loc
  locOf (Zero loc) = loc
  locOf (Top loc) = loc
