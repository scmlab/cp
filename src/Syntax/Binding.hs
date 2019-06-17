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


subsituteTypeVar :: Text -> Int -> Type -> Type
subsituteTypeVar name binder t = case t of
  Var (TypeVar var n l) loc -> if (Free name) == var
                      then Var (TypeVar (Bound binder) n l) loc
                      else Var (TypeVar var n l) loc
  _ -> t
