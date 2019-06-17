{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Concrete where

import Syntax.Base
import qualified Syntax.Abstract as A
import Syntax.Binding (Var(..))
import qualified Syntax.Binding as B

import Data.Loc (Loc(..), Located(..))
import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (on)
import qualified Data.Map as Map
import Prelude hiding (LT, EQ, GT)

import Control.Monad.State

--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

-- variables and names
data Name     = Name      Text Loc deriving (Show)
data Chan     = Chan      Text Loc deriving (Show)
data TypeVar  = TypeVar   Text Loc deriving (Show)
data TypeName = TypeName  Text Loc deriving (Show)


data Program = Program [Declaration] Loc deriving (Show)

data Declaration = TypeSig  Name Session Loc
                 | TermDefn Name Process Loc
                 deriving (Show)

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

data Session = Session (Map Chan Type) Loc deriving (Show)

-- instance Functor Session where
--   fmap f (Session p x) = Session (Map.mapKeys (fmap f) $ Map.map (fmap f) p) (f x)

insertSession :: Chan -> Type -> Session -> Session
insertSession x t (Session pairs m) =
    Session (Map.insert x t pairs) m

emptySession :: Loc -> Session
emptySession l = Session Map.empty l

singletonSession :: Chan -> Type -> Loc -> Session
singletonSession x t l = Session (Map.insert x t Map.empty) l

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

typeSigName :: Declaration -> Maybe Name
typeSigName (TypeSig n _ _) = Just n
typeSigName _               = Nothing

termDefnName :: Declaration -> Maybe Name
termDefnName (TermDefn n _ _) = Just n
termDefnName _                = Nothing

--------------------------------------------------------------------------------
-- | Instances

instance Eq Type where
  (==) = (==) `on` toAbstract . dual

instance Ord Type where
  compare = compare `on` toAbstract . dual

instance Eq Name where
  (==) = (==) `on` toAbstract

instance Ord Name where
  compare = compare `on` toAbstract

instance Eq Chan where
  (==) = (==) `on` toAbstract

instance Ord Chan where
  compare = compare `on` toAbstract

instance Eq TypeName where
  (==) = (==) `on` toAbstract

instance Ord TypeName where
  compare = compare `on` toAbstract

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


--------------------------------------------------------------------------------
-- | Converting to Abstract Syntax Tree

class ToAbstract a b | a -> b where
    toAbstract :: a -> b

instance ToAbstract Program A.Program where
    toAbstract (Program declarations _) =
        A.Program (map toAbstract declarations)

instance ToAbstract Declaration A.Declaration where
    toAbstract (TypeSig name session _) =
        A.TypeSig (toAbstract name) (toAbstract session)
    toAbstract (TermDefn name process _) =
        A.TermDefn (toAbstract name) (toAbstract process)

instance ToAbstract TypeVar A.TypeVar where
    toAbstract (TypeVar var _) =
        A.Named var

instance ToAbstract TypeName A.TypeName where
    toAbstract (TypeName name    _) = name

instance ToAbstract Name A.Name where
  toAbstract (Name name _) = name

instance ToAbstract Chan A.Chan where
    toAbstract (Chan name _) = name

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

instance ToAbstract Session A.Session where
    toAbstract (Session pairs _) = Map.mapKeys toAbstract $ Map.map toAbstract $ pairs

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

--------------------------------------------------------------------------------
-- | Converting to Concrete Binding Tree

data Binding = Binding
  { bindingBound :: Int
  , bindingFree :: Set Text
  } deriving (Show)

data BindingState = BindingState
  { bsChannel :: Binding
  , bsTypeVar :: Binding
  } deriving (Show)

type BindingM = State BindingState

class ToBinding a b | a -> b where
  toBindingM :: a -> BindingM b

toBinding :: ToBinding a b => a -> b
toBinding x =
  evalState
    (toBindingM x)
    (BindingState (Binding 0 Set.empty) (Binding 0 Set.empty))

boundTypeVar :: TypeVar -> BindingM (B.TypeVar, B.Type -> B.Type)
boundTypeVar (TypeVar name loc) = do
  Binding idx ns <- gets bsTypeVar
  modify $ \ st -> st { bsTypeVar = Binding (idx + 1) ns }
  let var = B.TypeVar (Bound idx) name loc
  return (var, B.subsituteTypeVar name idx)

freeTypeVar :: TypeVar -> BindingM B.TypeVar
freeTypeVar (TypeVar name loc) = do
  Binding idx ns <- gets bsTypeVar
  modify $ \ st -> st { bsTypeVar = Binding idx (Set.insert name ns) }
  return $ B.TypeVar (Free name) name loc

instance ToBinding Type B.Type where
  toBindingM (Var i loc) =
    B.Var
      <$> freeTypeVar i
      <*> pure loc
  toBindingM (Dual t loc) =
    B.Dual
      <$> toBindingM t
      <*> pure loc
  toBindingM (Times t u loc) =
    B.Times
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (Par t u loc) =
    B.Par
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (Plus t u loc) =
    B.Plus
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (With t u loc) =
    B.With
      <$> toBindingM t
      <*> toBindingM u
      <*> pure loc
  toBindingM (Acc t loc) =
    B.Acc
      <$> toBindingM t
      <*> pure loc
  toBindingM (Req t loc) =
    B.Req
      <$> toBindingM t
      <*> pure loc
  toBindingM (Exists x t loc) = do
    (var, bindFreeVars) <- boundTypeVar x
    t' <- bindFreeVars <$> toBindingM t
    B.Exists
      <$> pure var
      <*> pure t'
      <*> pure loc
  toBindingM (Forall x t loc) = do
    (var, bindFreeVars) <- boundTypeVar x
    t' <- bindFreeVars <$> toBindingM t
    B.Forall
      <$> pure var
      <*> pure t'
      <*> pure loc
  toBindingM (One loc) = B.One <$> pure loc
  toBindingM (Bot loc) = B.Bot <$> pure loc
  toBindingM (Zero loc) = B.Zero <$> pure loc
  toBindingM (Top loc) = B.Top <$> pure loc
