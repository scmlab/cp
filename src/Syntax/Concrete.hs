{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Concrete where

import Data.Text (Text)
import Data.Loc
import Prelude hiding (LT, EQ, GT)

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

data Name         ann = Name Text ann
                      deriving (Show, Functor)

data Program      ann = Program     [Declaration ann]                       ann
                      deriving (Show, Functor)

data Declaration  ann = Declaration  (Name ann)  (Process ann)              ann
                      deriving (Show, Functor)

data Process  ann = Link      (Name ann) (Name ann)                         ann
                  | Par       (Name ann) (Process ann) (Process ann)        ann
                  | Output    (Name ann) (Name ann)    (Process ann) (Process ann) ann
                  | Input     (Name ann) (Name ann)    (Process ann)        ann
                  | SelectL   (Name ann) (Process ann)                      ann
                  | SelectR   (Name ann) (Process ann)                      ann
                  | Choice    (Name ann) (Process ann) (Process ann)        ann
                  | Accept    (Name ann) (Name ann)    (Process ann)        ann
                  | Request   (Name ann) (Name ann)    (Process ann)        ann
                  | EmptyOutput          (Name ann)                         ann
                  | EmptyInput           (Name ann)    (Process ann)        ann
                  | EmptyChoice          (Name ann)                         ann
                  deriving (Show, Functor)

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located (Name Loc) where
  locOf (Name _ loc) = loc

instance Located (Program Loc) where
  locOf (Program _ loc) = loc

instance Located (Declaration Loc) where
  locOf (Declaration _ _ loc) = loc

instance Located (Process Loc) where
  locOf (Link _ _ loc) = loc
  locOf (Par _ _ _ loc) = loc
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
