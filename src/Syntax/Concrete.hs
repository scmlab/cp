{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Concrete where

import Syntax.Base
-- import qualified Syntax.Abstract as A
import qualified Syntax.Binding as B

import Data.Loc (Loc(..), Located(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Function (on)
import qualified Data.Map as Map
import Prelude hiding (LT, EQ, GT)

import Control.Monad.State

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

-- variables and names
data Name     = Name      Text Loc deriving (Show)
data Chan     = Chan      Text Loc deriving (Show, Eq, Ord)
data TypeVar  = TypeVar   Text Loc deriving (Show)
data TypeName = TypeName  Text Loc deriving (Show)


data Program = Program [Declaration] Loc deriving (Show)

data Declaration = TypeSig  Name SessionSyntax Loc
                 | TermDefn Name Process Loc
                 deriving (Show)

data SessionSyntax = SessionSyntax (Map Chan Type) Loc deriving (Show)

data Process  = Call      Name                              Loc
              | Link      Chan Chan                         Loc
              | Compose   Chan (Maybe Type) Process Process Loc
              | Output    Chan Chan Process Process         Loc
              | Input     Chan Chan Process                 Loc
              | SelectL   Chan Process                      Loc
              | SelectR   Chan Process                      Loc
              | Choice    Chan Process Process              Loc
              | Accept    Chan Chan Process                 Loc
              | Request   Chan Chan Process                 Loc
              | OutputT   Chan Type Process                 Loc
              | InputT    Chan TypeVar Process              Loc
              | EmptyOutput Chan                            Loc
              | EmptyInput  Chan Process                    Loc
              | EmptyChoice Chan                            Loc
              | End                                         Loc
              | Mix       Process   Process                 Loc
              deriving (Show)

insertSessionSyntax :: Chan -> Type -> SessionSyntax -> SessionSyntax
insertSessionSyntax x t (SessionSyntax pairs m) =
    SessionSyntax (Map.insert x t pairs) m

emptySessionSyntax :: Loc -> SessionSyntax
emptySessionSyntax l = SessionSyntax Map.empty l

singletonSessionSyntax :: Chan -> Type -> Loc -> SessionSyntax
singletonSessionSyntax x t l = SessionSyntax (Map.insert x t Map.empty) l

data Type = Var     TypeVar         Loc
          | Dual    Type            Loc
          | Times   Type      Type  Loc
          | Par     Type      Type  Loc
          | Plus    Type      Type  Loc
          | With    Type      Type  Loc
          | Acc     Type            Loc
          | Req     Type            Loc
          | Exists  TypeVar   Type  Loc
          | Forall  TypeVar   Type  Loc
          | One                     Loc
          | Bot                     Loc
          | Zero                    Loc
          | Top                     Loc
          deriving (Show)

instance HasDual Type where
  dual (Var i l)        = Dual (Var i l) l
  dual (Dual a _)       = a
  dual (Times a b l)    = Par (dual a) (dual b) l
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

--------------------------------------------------------------------------------
-- | Instances

-- instance Eq Type where
--   (==) = (==) `on` toBinding . dual
--
-- instance Ord Type where
--   compare = compare `on` toBinding . dual
--
-- instance Eq Name where
--   (==) = (==) `on` toBinding
--
-- instance Ord Name where
--   compare = compare `on` toBinding
--
--
-- instance Eq TypeName where
--   (==) = (==) `on` toBinding
--
-- instance Ord TypeName where
--   compare = compare `on` toBinding

--------------------------------------------------------------------------------
-- | Instance of Located

instance Located TypeName where
  locOf (TypeName _ loc) = loc

instance Located Name where
  locOf (Name _ loc) = loc

instance Located Chan where
  locOf (Chan _ loc) = loc

instance Located Program where
  locOf (Program _ loc) = loc

instance Located Declaration where
  locOf (TypeSig _ _ loc) = loc
  locOf (TermDefn _ _ loc) = loc

instance Located Process where
  locOf (Call _ loc) = loc
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
  locOf (End loc) = loc
  locOf (Mix _ _ loc) = loc

instance Located Type where
  locOf (Var _ loc) = loc
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
