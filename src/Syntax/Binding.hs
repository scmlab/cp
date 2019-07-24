{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Binding
    ( module Syntax.Binding
    , module Syntax.Base)
    where

import Syntax.Base
-- import qualified Syntax.Abstract as A

import Data.Loc (Loc(..), Located(..))
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import Data.Function (on)

--------------------------------------------------------------------------------
-- | Type Variable

data TypeVar  = TypeVar Text Loc
              | Unknown
              deriving (Show)
--
-- instance Show TypeVar where
--     show (TypeVar i) = "$" ++ show i
--     show (Named n) = show n
--     show Unknown = "?"
--     show (DualOf v) = "^" ++ show v

instance Eq TypeVar where
    -- Unknown is equivalent to anything
    Unknown == _ = True
    _ == Unknown = True
    --
    TypeVar i _ == TypeVar j _ = i == j
    _ == _ = False

instance Ord TypeVar where
    Unknown `compare` Unknown = EQ
    Unknown `compare` _       = LT
    _       `compare` Unknown = GT
    TypeVar i _ `compare` TypeVar j _ = i `compare` j

--------------------------------------------------------------------------------
-- | Concrete Binding Tree

-- Names
data Name     = Name      Text Loc deriving (Show)
data TypeName = TypeName  Text Loc deriving (Show)

--
data Chan     = Chan Text Loc deriving (Show)

chanName :: Chan -> Text
chanName (Chan name _) = name

data Definition = Paired   Name Process Session
                | TypeOnly Name Session
                | TermOnly Name Process
                deriving (Show)
type Definitions = Map Name Definition
data Program = Program Definitions Loc deriving (Show)

-- data Declaration = TypeSig  Name SessionSyntax Loc
--                  | TermDefn Name Process Loc
--                  deriving (Show)

-- typeSigName :: Declaration -> Maybe Name
-- typeSigName (TypeSig n _ _) = Just n
-- typeSigName _               = Nothing
--
-- termDefnName :: Declaration -> Maybe Name
-- termDefnName (TermDefn n _ _) = Just n
-- termDefnName _                = Nothing

type Session = Map Chan Type
data SessionSyntax = SessionSyntax Session Loc deriving (Show)

type FreeChans = Set Text

data Process = Process Proc FreeChans Loc
    deriving (Eq, Ord, Show)

data Proc = Atom      Name FreeChans
          | Link      Chan Chan
          | Compose   Chan (Maybe Type) Process Process
          | Output    Chan Chan Process Process
          | Input     Chan Chan Process
          | SelectL   Chan Process
          | SelectR   Chan Process
          | Choice    Chan Process Process
          | Accept    Chan Chan Process
          | Request   Chan Chan Process
          | OutputT   Chan Type Process
          | InputT    Chan TypeVar Process
          | EmptyOutput Chan
          | EmptyInput  Chan Process
          | EmptyChoice Chan
          | End
          | Mix       Process   Process
          deriving (Eq, Ord, Show)

toProc :: Process -> Proc
toProc (Process p _ _) = p

data Type
    = Var TypeVar Loc
    -- Subst B X A: B { A / X }
    | Dual Type Loc
    -- A ⊗ B: output A then behave as B
    | Times Type Type Loc
    -- A 􏰂􏰂􏰂⅋ B: input A then behave as B
    | Par Type Type Loc
    -- A ⊕ B: select A or B
    | Plus Type Type Loc
    -- A & B: offer choice of A or B
    | With Type Type Loc
    -- of cource!
    | Acc Type Loc
    -- why not?
    | Req Type Loc
    -- existential:   snd = B {fst / X}
    | Exists
        TypeVar   -- X: the type variable to be substituted
        Type      -- B: the type (variable) representing the type before substitution
        (Maybe (Type, Type)) -- extra information, useful when composing with Forall
           -- fst: the type to be substituted with
           -- snd: the resulting type after substitution
        Loc
    -- universal
    | Forall TypeVar Type Loc
    -- 1: unit for Times ⊗
    | One Loc
    -- ⊥: unit for Par ⅋
    | Bot Loc
    -- 0: unit for Plus ⊕
    | Zero Loc
    -- ⊤: unit for With &
    | Top Loc
    deriving (Show)

-- data Type = Var     TypeVar         Loc
--           | Dual    Type            Loc
--           | Times   Type      Type  Loc
--           | Par     Type      Type  Loc
--           | Plus    Type      Type  Loc
--           | With    Type      Type  Loc
--           | Acc     Type            Loc
--           | Req     Type            Loc
--           | Exists  TypeVar   Type  Loc
--           | Forall  TypeVar   Type  Loc
--           | One                     Loc
--           | Bot                     Loc
--           | Zero                    Loc
--           | Top                     Loc
--           deriving (Show)

--------------------------------------------------------------------------------
-- | Instances

instance Eq Type where
  (==) = eq `on` dual
      where
        eq :: Type -> Type -> Bool
        eq (Var a _) (Var b _) = a == b
        eq (Dual a _) (Dual b _) = eq a b
        eq (Times a b _) (Times c d _) = eq a c && eq b d
        eq (Par a b _) (Par c d _) = eq a c && eq b d
        eq (Plus a b _) (Plus c d _) = eq a c && eq b d
        eq (With a b _) (With c d _) = eq a c && eq b d
        eq (Acc a _) (Acc b _) = eq a b
        eq (Req a _) (Req b _) = eq a b
        eq (Exists a b c _) (Exists d e f _) = a == d && eq b e && c == f
        eq (Forall a b _) (Forall c d _) = a == c && eq b d
        eq (One _) (One _) = True
        eq (Bot _) (Bot _) = True
        eq (Zero _) (Zero _) = True
        eq (Top _) (Top _) = True
        eq _ _ = False

instance Ord Type where
  compare _ _ = EQ

instance Eq Name where
  (Name a _) == (Name b _) = a == b

instance Ord Name where
  (Name a _) `compare` (Name b _) = a `compare` b

instance Eq Chan where
  (==) = (==) `on` chanName

instance Ord Chan where
  compare = compare `on` chanName

-- instance Eq TypeName where
--   (==) = (==) `on` toAbstract
--
-- instance Ord TypeName where
--   compare = compare `on` toAbstract
--------------------------------------------------------------------------------
-- | Make free variables bounded

subsituteTypeVar :: Text -> Text -> TypeVar -> TypeVar
subsituteTypeVar old new (TypeVar name loc)
  | old == name = TypeVar new loc
  | otherwise   = TypeVar old loc
subsituteTypeVar _ _ Unknown = Unknown

subsituteType :: Text -> Text -> Type -> Type
subsituteType old new (Var var loc) = Var (subsituteTypeVar old new var) loc
subsituteType _   _   others        = others

subsituteFreeChans :: Text -> Text -> FreeChans -> FreeChans
subsituteFreeChans old new = Set.map (subst old new)
  where
    subst :: Text -> Text -> Text -> Text
    subst old new name
      | old == name = new
      | otherwise   = old

subsituteChannel :: Text -> Text -> Chan -> Chan
subsituteChannel old new (Chan name loc)
  | old == name = Chan old loc
  | otherwise   = Chan new loc
subsituteProcess :: Text -> Text -> Process -> Process
subsituteProcess old new (Process proc free loc) =
  Process
    (subsituteProc old new proc)
    (subsituteFreeChans old new free)
    loc

subsituteProc :: Text -> Text -> Proc -> Proc
subsituteProc old new process = case process of
  Atom name chans -> Atom name chans
  Link x y -> Link (chan x) (chan y)
  Compose x t a b -> Compose (chan x) t (proc a) (proc b)
  Output x y a b -> Output (chan x) (chan y) (proc a) (proc b)
  Input x y a -> Input (chan x) (chan y) (proc a)
  SelectL x a -> SelectL (chan x) (proc a)
  SelectR x a -> SelectR (chan x) (proc a)
  Choice x a b -> Choice (chan x) (proc a) (proc b)
  Accept x y a -> Accept (chan x) (chan y) (proc a)
  Request x y a -> Request (chan x) (chan y) (proc a)
  OutputT x t a -> OutputT (chan x) t (proc a)
  InputT x t a -> InputT (chan x) t (proc a)
  EmptyOutput x -> EmptyOutput (chan x)
  EmptyInput x a -> EmptyInput (chan x) (proc a)
  EmptyChoice x -> EmptyChoice (chan x)
  End -> End
  Mix p q -> Mix (proc p) (proc q)
  where
    chan = subsituteChannel old new
    proc = subsituteProcess old new


--------------------------------------------------------------------------------
-- | Instance of Located

instance Located TypeVar where
  locOf (TypeVar _ loc) = loc
  locOf Unknown         = NoLoc

instance Located TypeName where
  locOf (TypeName _ loc) = loc

instance Located Name where
  locOf (Name _ loc) = loc

instance Located Chan where
  locOf (Chan _ loc) = loc

instance Located Program where
  locOf (Program _ loc) = loc

instance Located Process where
  locOf (Process _ _ loc) = loc

instance Located Type where
  locOf (Var _ loc) = loc
  locOf (Dual _ loc) = loc
  locOf (Times _ _ loc) = loc
  locOf (Par _ _ loc) = loc
  locOf (Plus _ _ loc) = loc
  locOf (With _ _ loc) = loc
  locOf (Acc _ loc) = loc
  locOf (Req _ loc) = loc
  locOf (Exists _ _ _ loc) = loc
  locOf (Forall _ _ loc) = loc
  locOf (One loc) = loc
  locOf (Bot loc) = loc
  locOf (Zero loc) = loc
  locOf (Top loc) = loc

--------------------------------------------------------------------------------
-- | Instance of HasDual

-- instance HasDual TypeVar where
--   dual (TypeVar v n l)  = DualOf (TypeVar v n l)
--   dual Unknown          = Unknown
  -- dual (DualOf v)       = v

instance HasDual Type where
  dual (Var a l)          = Dual (Var a l) l
  dual (Dual a _)         = a
  dual (Times a b l)      = Par (dual a) (dual b) l
  dual (Par a b l)        = Times (dual a) (dual b) l
  dual (Plus a b l)       = With (dual a) (dual b) l
  dual (With a b l)       = Plus (dual a) (dual b) l
  dual (Acc a l)          = Req (dual a) l
  dual (Req a l)          = Acc (dual a) l
  dual (Exists x a _ l)   = Forall x (dual a) l
  dual (Forall x a l)     = Exists x (dual a) Nothing l
  dual (One l)            = Bot l
  dual (Bot l)            = One l
  dual (Zero l)           = Top l
  dual (Top l)            = Zero l


convert :: SessionSyntax -> Session
convert (SessionSyntax xs _) = xs

--------------------------------------------------------------------------------
-- Free variables

freeChans :: Process -> FreeChans
freeChans (Process p xs _) = xs
  -- where
  --
  --
  --   case p of
freeChans' :: Proc -> FreeChans
freeChans' p = case p of
  Atom _ xs -> xs
  Link x y -> Set.fromList [chanName x, chanName y]
  Compose x _ p q -> Set.delete (chanName x) $ Set.union (freeChans p) (freeChans q)
  Output x y p q -> Set.insert (chanName x) $ Set.delete (chanName y) $ Set.union (freeChans p) (freeChans q)
  Input x y p -> Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
  SelectL x p -> Set.insert (chanName x) $ freeChans p
  SelectR x p -> Set.insert (chanName x) $ freeChans p
  Choice x p q -> Set.insert (chanName x) $ Set.union (freeChans p) (freeChans q)
  Accept x y p ->Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
  Request x y p -> Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
  OutputT x _ p -> Set.insert (chanName x) (freeChans p)
  InputT x _ p -> Set.insert (chanName x) (freeChans p)
  EmptyOutput x -> Set.singleton (chanName x)
  EmptyInput x p -> Set.insert (chanName x) (freeChans p)
  EmptyChoice x -> Set.singleton (chanName x)
  End -> Set.empty
  Mix p q -> Set.union (freeChans p) (freeChans q)
