-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- {-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Binding where

-- import Syntax.Base
-- import qualified Syntax.Abstract as A

import Data.Loc (Loc(..))
import Data.Text (Text)
-- import Data.Map (Map)
-- import Data.Function (on)
-- import qualified Data.Map as Map
-- import Prelude hiding (LT, EQ, GT)

--------------------------------------------------------------------------------
-- | Concrete Binding Tree

-- Names
data Name     = Name      Text Loc deriving (Show)
data TypeName = TypeName  Text Loc deriving (Show)

--
data Var = Free Text
         | Bound Int
         deriving (Eq, Show)

data Chan     = Chan Var Text Loc
              deriving (Eq, Show)
data TypeVar  = TypeVar Var Text Loc
              deriving (Show)

instance Eq TypeVar where
  (TypeVar a _ _) == (TypeVar b _ _) = a == b

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

subsituteTypeVar :: Text -> Int -> TypeVar -> TypeVar
subsituteTypeVar free bound (TypeVar var name loc)
  | Free free == var = TypeVar (Bound bound) name loc
  | otherwise        = TypeVar var name loc

-- subsituteTypeVar
subsituteType :: Text -> Int -> Type -> Type
subsituteType free bound (Var var loc) = Var (subsituteTypeVar free bound var) loc
subsituteType _    _     others        = others

subsituteChannel :: Text -> Int -> Chan -> Chan
subsituteChannel free bound (Chan var name loc)
  | Free free == var = Chan (Bound bound) name loc
  | otherwise        = Chan var name loc

subsituteProcess :: Text -> Int -> Process -> Process
subsituteProcess free bound process = case process of
  Link x y loc -> Link (subst x) (subst y) loc
  Compose x t a b loc -> Compose (subst x) t a b loc
  Output x y a b loc -> Output (subst x) (subst y) a b loc
  Input x y a loc -> Input (subst x) (subst y) a loc
  SelectL x a loc -> SelectL (subst x) a loc
  SelectR x a loc -> SelectR (subst x) a loc
  Choice x a b loc -> Choice (subst x) a b loc
  Accept x y a loc -> Accept (subst x) (subst y) a loc
  Request x y a loc -> Request (subst x) (subst y) a loc
  OutputT x t a loc -> OutputT (subst x) t a loc
  InputT x t a loc -> InputT (subst x) t a loc
  EmptyOutput x loc -> EmptyOutput (subst x) loc
  EmptyInput x a loc -> EmptyInput (subst x) a loc
  EmptyChoice x loc -> EmptyChoice (subst x) loc
  others -> others
  where
    subst = subsituteChannel free bound

-- subsituteChannel :: Text -> Int -> Process -> Process
-- subsituteChannel free bound p = case p of
--   (Link a b l) ->


-- subsituteChannel :: Text -> Int -> Process -> Process
-- subsituteChannel name binder t = case t of
--   Var (Chan var n l) m loc -> if (Free name) == var
--                       then Var (Chan (Bound binder) n l) m loc
--                       else Var (Chan var n l) m loc
--   _ -> t
