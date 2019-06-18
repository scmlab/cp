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
data Chan     = Chan      Text Loc deriving (Show)
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
instance Eq Chan where
  (==) = (==) `on` toBinding

instance Ord Chan where
  compare = compare `on` toBinding
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


-- --------------------------------------------------------------------------------
-- -- | Converting to Abstract Syntax Tree
--
-- instance ToAbstract TypeVar A.TypeVar where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract TypeName A.TypeName where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract Name A.Name where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract Chan A.Chan where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract Program A.Program where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract Declaration A.Declaration where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract Session A.Session where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract Process A.Process where
--   toAbstract = toAbstract . toBinding
--
-- instance ToAbstract Type A.Type where
--   toAbstract = toAbstract . toBinding

--------------------------------------------------------------------------------

createBinderTypeVar :: TypeVar -> BindingM (B.TypeVar, B.Type -> B.Type)
createBinderTypeVar (TypeVar name loc) = do
  Binding idx ns <- gets bsTypeVar
  modify $ \ st -> st { bsTypeVar = Binding (idx + 1) ns }
  let var = B.TypeVar (Bound idx) name loc
  return (var, B.subsituteType name idx)

instance ToBinding TypeVar B.TypeVar where
  toBindingM (TypeVar name loc) = do
    Binding idx ns <- gets bsTypeVar
    modify $ \ st -> st { bsTypeVar = Binding idx (Set.insert name ns) }
    return $ B.TypeVar (Free name) name loc

instance ToBinding Type B.Type where
  toBindingM (Var i loc) =
    B.Var
      <$> toBindingM i
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
    (var, bindFreeVars) <- createBinderTypeVar x
    t' <- bindFreeVars <$> toBindingM t
    return $ B.Exists var t' Nothing loc
  toBindingM (Forall x t loc) = do
    (var, bindFreeVars) <- createBinderTypeVar x
    t' <- bindFreeVars <$> toBindingM t
    return $ B.Forall var t' loc
  toBindingM (One loc) = B.One <$> pure loc
  toBindingM (Bot loc) = B.Bot <$> pure loc
  toBindingM (Zero loc) = B.Zero <$> pure loc
  toBindingM (Top loc) = B.Top <$> pure loc

--------------------------------------------------------------------------------

-- creates a channcel binder along with a function that binds free variables
createBinderChan :: Chan -> BindingM (B.Chan, B.Process -> B.Process)
createBinderChan (Chan name loc) = do
  Binding idx ns <- gets bsChannel
  modify $ \ st -> st { bsChannel = Binding (idx + 1) ns }
  let var = B.Chan (Bound idx) name loc
  return (var, B.subsituteProcess name idx)


instance ToBinding TypeName B.TypeName where
  toBindingM (TypeName name loc) = return $ B.TypeName name loc

instance ToBinding Name B.Name where
  toBindingM (Name name loc) = return $ B.Name name loc

instance ToBinding Chan B.Chan where
  toBindingM (Chan name loc) = do
    Binding idx ns <- gets bsChannel
    modify $ \ st -> st { bsChannel = Binding idx (Set.insert name ns) }
    return $ B.Chan (Free name) name loc

instance ToBinding Process B.Process where
  toBindingM (Call name loc) =
    B.Call
      <$> toBindingM name
      <*> pure loc
  toBindingM (Link nameA nameB loc) =
    B.Link
      <$> toBindingM nameA
      <*> toBindingM nameB
      <*> pure loc
  toBindingM (Compose x Nothing procA procB loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> pure Nothing
      <*> (bind <$> toBindingM procA)
      <*> (bind <$> toBindingM procB)
      <*> pure loc
  toBindingM (Compose x (Just t) procA procB loc) = do
    (var, bind) <- createBinderChan x
    B.Compose
      <$> pure var
      <*> (Just <$> toBindingM t)
      <*> (bind <$> toBindingM procA)
      <*> (bind <$> toBindingM procB)
      <*> pure loc
  toBindingM (Output nameA nameB procB procA loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Output
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM procB)
      <*> toBindingM procA
      <*> pure loc
  toBindingM (Input nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Input
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM proc)
      <*> pure loc
  toBindingM (SelectL name proc loc) =
    B.SelectL
      <$> toBindingM name
      <*> toBindingM proc
      <*> pure loc
  toBindingM (SelectR name proc loc) =
    B.SelectR
      <$> toBindingM name
      <*> toBindingM proc
      <*> pure loc
  toBindingM (Choice name procA procB loc) =
    B.Choice
      <$> toBindingM name
      <*> toBindingM procA
      <*> toBindingM procB
      <*> pure loc
  toBindingM (Accept nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Accept
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM proc)
      <*> pure loc
  toBindingM (Request nameA nameB proc loc) = do
    (varB, bindB) <- createBinderChan nameB
    B.Request
      <$> toBindingM nameA
      <*> pure varB
      <*> (bindB <$> toBindingM proc)
      <*> pure loc
  toBindingM (OutputT name typ proc loc) =
    B.OutputT
      <$> toBindingM name
      <*> toBindingM typ
      <*> toBindingM proc
      <*> pure loc
  toBindingM (InputT name (TypeVar n l) proc loc) = do
    B.InputT
      <$> toBindingM name
      <*> pure (B.TypeVar (Free n) n l)
      <*> toBindingM proc
      <*> pure loc
  toBindingM (EmptyOutput name loc) =
    B.EmptyOutput
      <$> toBindingM name
      <*> pure loc
  toBindingM (EmptyInput name proc loc) =
    B.EmptyInput
      <$> toBindingM name
      <*> toBindingM proc
      <*> pure loc
  toBindingM (EmptyChoice name loc) =
    B.EmptyChoice
      <$> toBindingM name
      <*> pure loc
  toBindingM (End loc) =
    B.End
      <$> pure loc
  toBindingM (Mix p q loc) =
    B.Mix
      <$> toBindingM p
      <*> toBindingM q
      <*> pure loc

--------------------------------------------------------------------------------

instance ToBinding Program B.Program where
  toBindingM (Program declarations loc) =
    B.Program
      <$> mapM toBindingM declarations
      <*> pure loc

instance ToBinding Declaration B.Declaration where
  toBindingM (TypeSig name session loc) =
    B.TypeSig
      <$> toBindingM name
      <*> toBindingM session
      <*> pure loc
  toBindingM (TermDefn name process loc) =
    B.TermDefn
      <$> toBindingM name
      <*> toBindingM process
      <*> pure loc

instance ToBinding SessionSyntax B.SessionSyntax where
  toBindingM (SessionSyntax pairs loc) = do
    let (keys, elems) = unzip $ Map.toList pairs
    keys' <- mapM toBindingM keys
    elems' <- mapM toBindingM elems
    let pairs' = Map.fromList (zip keys' elems')
    return $ B.SessionSyntax pairs' loc
