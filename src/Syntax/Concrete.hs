{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Concrete where

import qualified Syntax.Abstract as A

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

data Declaration  ann = TypeSig   (TermName ann)  (Type ann)                ann
                      | TermDefn  (TermName ann)  (Process ann)             ann
                      deriving (Show, Functor)

data Process  ann = Link      (TermName ann) (TermName ann)                 ann
                  | Compose   (TermName ann) (Type     ann) (Process  ann) (Process ann)   ann
                  | Output    (TermName ann) (TermName ann) (Process ann) (Process ann) ann
                  | Input     (TermName ann) (TermName ann) (Process ann)   ann
                  | SelectL   (TermName ann) (Process  ann)                 ann
                  | SelectR   (TermName ann) (Process  ann)                 ann
                  | Choice    (TermName ann) (Process  ann) (Process ann)   ann
                  | Accept    (TermName ann) (TermName ann) (Process ann)   ann
                  | Request   (TermName ann) (TermName ann) (Process ann)   ann
                  | OutputT   (TermName ann) (TypeName ann) (Process ann)   ann
                  | InputT    (TermName ann) (TypeName ann) (Process ann)   ann
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

instance HasDual (Type ann) where
  dual (Dual a _)       = a
  dual (Times a b l)      = Par (dual a) (dual b) l
  dual (Par a b l)      = Times (dual a) (dual b) l
  dual (Plus a b l)     = With (dual a) (dual b) l
  dual (With a b l)     = Plus (dual a) (dual b) l
  dual (Acc a l)        = Req (dual a) l
  dual (Req a l)        = Acc (dual a) l
  dual (Exists x a l)   = Forall x (dual a) l
  dual (Forall x a l)   = Exists x (dual a) l
  dual (One l)          = Bot l
  dual (Bot l)          = One l
  dual (Zero l)         = Top l
  dual (Top l)          = Zero l

typeSigName :: Declaration ann -> Maybe (TermName ann)
typeSigName (TypeSig n _ _) = Just n
typeSigName _             = Nothing

termDefnName :: Declaration ann -> Maybe (TermName ann)
termDefnName (TermDefn n _ _) = Just n
termDefnName _              = Nothing

instance Eq (Type Loc) where
  (==) a b = toAbstract (dual a) == toAbstract (dual b)

--------------------------------------------------------------------------------
-- | Instances

instance Eq (TermName Loc) where
  (==) (TermName a _) (TermName b _) = a == b

instance Ord (TermName Loc) where
  compare (TermName a _) (TermName b _) = compare a b

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located (TypeName Loc) where
  locOf (TypeName _ loc) = loc

instance Located (TermName Loc) where
  locOf (TermName _ loc) = loc

instance Located (Program Loc) where
  locOf (Program _ loc) = loc

instance Located (Declaration Loc) where
  locOf (TypeSig _ _ loc) = loc
  locOf (TermDefn _ _ loc) = loc

instance Located (Process Loc) where
  locOf (Link _ _ loc) = loc
  locOf (Compose _ _ _ _ loc) = loc
  locOf (Output _ _ _ _ loc) = loc
  locOf (Input _ _ _ loc) = loc
  locOf (SelectL _ _ loc) = loc
  locOf (SelectR _ _ loc) = loc
  locOf (Choice _ _ _ loc) = loc
  locOf (Accept _ _ _ loc) = loc
  locOf (Request _ _ _ loc) = loc
  locOf (OutputT _ _ _ loc) = loc
  locOf (InputT _ _ _ loc) = loc
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


--------------------------------------------------------------------------------
-- | Converting to Abstract Syntax Tree

class ToAbstract a b | a -> b where
    toAbstract :: a -> b

instance ToAbstract (Program ann) A.Program where
    toAbstract (Program  declarations _) =
        A.Program (map toAbstract declarations)

instance ToAbstract (Declaration ann) A.Declaration where
    toAbstract (TypeSig name typ _) =
        A.TypeSig (toAbstract name) (toAbstract typ)
    toAbstract (TermDefn name process _) =
        A.TermDefn (toAbstract name) (toAbstract process)

instance ToAbstract (TypeName ann) A.TypeName where
    toAbstract (TypeName name    _) = name

instance ToAbstract (TermName ann) A.TermName where
    toAbstract (TermName name    _) = name

instance ToAbstract (Process ann) A.Process where
    toAbstract (Link nameA nameB _) =
        A.Link
            (toAbstract nameA)
            (toAbstract nameB)
    toAbstract (Compose name typ procA procB _) =
        A.Compose
            (toAbstract name)
            (toAbstract typ)
            (toAbstract procA)
            (toAbstract procB)
    toAbstract (Output nameA nameB procA procB _) =
        A.Output
            (toAbstract nameA)
            (toAbstract nameB)
            (toAbstract procA)
            (toAbstract procB)
    toAbstract (Input nameA nameB proc _) =
        A.Input
            (toAbstract nameA)
            (toAbstract nameB)
            (toAbstract proc)
    toAbstract (SelectL name proc _) =
        A.SelectL
            (toAbstract name)
            (toAbstract proc)
    toAbstract (SelectR name proc _) =
        A.SelectR
            (toAbstract name)
            (toAbstract proc)
    toAbstract (Choice name procA procB _) =
        A.Choice
            (toAbstract name)
            (toAbstract procA)
            (toAbstract procB)
    toAbstract (Accept nameA nameB proc _) =
        A.Accept
            (toAbstract nameA)
            (toAbstract nameB)
            (toAbstract proc)
    toAbstract (Request nameA nameB proc _) =
        A.Request
            (toAbstract nameA)
            (toAbstract nameB)
            (toAbstract proc)
    toAbstract (OutputT name typ proc _) =
        A.OutputT
            (toAbstract name)
            (toAbstract typ)
            (toAbstract proc)
    toAbstract (InputT name typ proc _) =
        A.InputT
            (toAbstract name)
            (toAbstract typ)
            (toAbstract proc)
    toAbstract (EmptyOutput name _) =
        A.EmptyOutput
            (toAbstract name)
    toAbstract (EmptyInput name proc _) =
        A.EmptyInput
            (toAbstract name)
            (toAbstract proc)
    toAbstract (EmptyChoice name _) =
        A.EmptyChoice
            (toAbstract name)

instance ToAbstract (Type ann) A.Type where
    toAbstract (Dual t _) =
        A.Dual
            (toAbstract t)
    toAbstract (Times t u _) =
        A.Times
            (toAbstract t)
            (toAbstract u)
    toAbstract (Par t u _) =
        A.Par
            (toAbstract t)
            (toAbstract u)
    toAbstract (Plus t u _) =
        A.Plus
            (toAbstract t)
            (toAbstract u)
    toAbstract (With t u _) =
        A.With
            (toAbstract t)
            (toAbstract u)
    toAbstract (Acc t _) =
        A.Acc
            (toAbstract t)
    toAbstract (Req t _) =
        A.Req
            (toAbstract t)
    toAbstract (Exists x t _) =
        A.Exists
            (toAbstract x)
            (toAbstract t)
    toAbstract (Forall x t _) =
        A.Forall
            (toAbstract x)
            (toAbstract t)
    toAbstract (One _) = A.One
    toAbstract (Bot _) = A.Bot
    toAbstract (Zero _) = A.Zero
    toAbstract (Top _) = A.Top

--------------------------------------------------------------------------------
-- | Duality

class HasDual a where
  dual :: a -> a

instance HasDual A.Type where
    dual (A.Dual a)       = a
    dual (A.Times a b)    = A.Par (dual a) (dual b)
    dual (A.Par a b)      = A.Times (dual a) (dual b)
    dual (A.Plus a b)     = A.With (dual a) (dual b)
    dual (A.With a b)     = A.Plus (dual a) (dual b)
    dual (A.Acc a)        = A.Req (dual a)
    dual (A.Req a)        = A.Acc (dual a)
    dual (A.Exists x a)   = A.Forall x (dual a)
    dual (A.Forall x a)   = A.Exists x (dual a)
    dual A.One            = A.Bot
    dual A.Bot            = A.One
    dual A.Zero           = A.Top
    dual A.Top            = A.Zero
