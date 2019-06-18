{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Binding where

import Syntax.Base
import qualified Syntax.Abstract as A

import Data.Loc (Loc(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Function (on)


--------------------------------------------------------------------------------
-- | Concrete Binding Tree

-- Names
data Name     = Name      Text Loc deriving (Show)
data TypeName = TypeName  Text Loc deriving (Show)

--
data Chan     = Chan Var Text Loc
              deriving (Show)

data TypeVar  = TypeVar Var Text Loc
              deriving (Show)

instance Eq TypeVar where
  (TypeVar a _ _) == (TypeVar b _ _) = a == b

data Program = Program [Declaration] Loc deriving (Show)

data Declaration = TypeSig  Name Session Loc
                 | TermDefn Name Process Loc
                 deriving (Show)

data Session = Session (Map Chan Type) Loc deriving (Show)

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

--------------------------------------------------------------------------------
-- | Instances

-- instance Eq Type where
--   (==) = (==) `on` toAbstract . dual
--
-- instance Ord Type where
--   compare = compare `on` toAbstract . dual
--
-- instance Eq Name where
--   (==) = (==) `on` toAbstract
--
-- instance Ord Name where
--   compare = compare `on` toAbstract

instance Eq Chan where
  (==) = (==) `on` toAbstract

instance Ord Chan where
  compare = compare `on` toAbstract

-- instance Eq TypeName where
--   (==) = (==) `on` toAbstract
--
-- instance Ord TypeName where
--   compare = compare `on` toAbstract
--------------------------------------------------------------------------------
-- | subsitution

subsituteTypeVar :: Text -> Int -> TypeVar -> TypeVar
subsituteTypeVar free bound (TypeVar var name loc)
  | Free free == var = TypeVar (Bound bound) name loc
  | otherwise        = TypeVar var name loc

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

--------------------------------------------------------------------------------
-- | Converting to Abstract Binding Tree

class ToAbstract a b | a -> b where
  toAbstract :: a -> b

instance ToAbstract TypeVar A.TypeVar where
    toAbstract (TypeVar var name _) = A.Named name

instance ToAbstract TypeName A.TypeName where
    toAbstract (TypeName name _) = name

instance ToAbstract Name A.Name where
  toAbstract (Name name _) = name

instance ToAbstract Chan A.Chan where
  toAbstract (Chan var name _) = name

instance ToAbstract Program A.Program where
  toAbstract (Program declarations _) =
    A.Program (map toAbstract declarations)

instance ToAbstract Declaration A.Declaration where
    toAbstract (TypeSig name session _) =
        A.TypeSig (toAbstract name) (toAbstract session)
    toAbstract (TermDefn name process _) =
        A.TermDefn (toAbstract name) (toAbstract process)

instance ToAbstract Session A.Session where
    toAbstract (Session pairs _) = Map.mapKeys toAbstract $ Map.map toAbstract $ pairs

instance ToAbstract Process A.Process where
    toAbstract (Call name _) =
        A.Call
            (toAbstract name)
    toAbstract (Link nameA nameB _) =
        A.Link
            (toAbstract nameA)
            (toAbstract nameB)
    toAbstract (Compose name Nothing procA procB _) =
        A.Compose
            (toAbstract name)
            Nothing
            (toAbstract procA)
            (toAbstract procB)
    toAbstract (Compose name (Just t) procA procB _) =
        A.Compose
            (toAbstract name)
            (Just (toAbstract t))
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
    toAbstract (End _) =
        A.End
    toAbstract (Mix p q _) =
        A.Mix
            (toAbstract p)
            (toAbstract q)

instance ToAbstract Type A.Type where
    toAbstract (Var i _) =
        A.Var
            (toAbstract i)
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
            Nothing
    toAbstract (Forall x t _) =
        A.Forall
            (toAbstract x)
            (toAbstract t)
    toAbstract (One _) = A.One
    toAbstract (Bot _) = A.Bot
    toAbstract (Zero _) = A.Zero
    toAbstract (Top _) = A.Top


-- instance ToAbstract TypeVar A.TypeVar where
--     toAbstract (TypeVar var _) =
--         A.Named var
--
-- instance ToAbstract TypeName A.TypeName where
--     toAbstract (TypeName name    _) = name
--
-- instance ToAbstract Name A.Name where
--   toAbstract (Name name _) = name
--
-- instance ToAbstract Chan A.Chan where
--     toAbstract (Chan name _) = name
