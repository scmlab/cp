{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

module Syntax.Binding where

import Syntax.Base
-- import qualified Syntax.Abstract as A

import Data.Loc (Loc(..), Located(..))
import Data.Text (Text)
import Data.Map (Map)
import Data.Function (on)

--------------------------------------------------------------------------------
-- | Type Variable

data TypeVar  = TypeVar Var Text Loc
              | Unknown
              | DualOf TypeVar
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
    TypeVar i _ _ == TypeVar j _ _ = i == j
    DualOf m == DualOf n = m == n
    --
    _ == _ = False

instance Ord TypeVar where
    Unknown `compare` Unknown = EQ
    Unknown `compare` _       = LT
    _       `compare` Unknown = GT
    --
    TypeVar i _ _ `compare` TypeVar j _ _ = i `compare` j
    DualOf m `compare` DualOf n = m `compare` n
    -- whatever ???
    _ `compare` _ = EQ

--------------------------------------------------------------------------------
-- | Concrete Binding Tree

-- Names
data Name     = Name      Text Loc deriving (Show)
data TypeName = TypeName  Text Loc deriving (Show)

--
data Chan     = Chan Var Text Loc
              deriving (Show)

data Program = Program [Declaration] Loc deriving (Show)

data Declaration = TypeSig  Name SessionSyntax Loc
                 | TermDefn Name Process Loc
                 deriving (Show)

typeSigName :: Declaration -> Maybe Name
typeSigName (TypeSig n _ _) = Just n
typeSigName _               = Nothing

termDefnName :: Declaration -> Maybe Name
termDefnName (TermDefn n _ _) = Just n
termDefnName _                = Nothing

type Session = Map Chan Type
data SessionSyntax = SessionSyntax Session Loc deriving (Show)

convert :: SessionSyntax -> Session
convert (SessionSyntax xs _) = xs

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
              deriving (Eq, Ord, Show)

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
  (==) = (==) `on` dual

instance Ord Type where
  compare = compare `on` dual

instance Eq Name where
  (Name a _) == (Name b _) = a == b

instance Ord Name where
  (Name a _) `compare` (Name b _) = a `compare` b

-- TODO: FIX THIS
instance Eq Chan where
  -- (Chan varA _ _) == (Chan varB _ _) = varA == varB
  (Chan _ nameA _) == (Chan _ nameB _) = nameA == nameB

instance Ord Chan where
  (Chan _ nameA _) `compare` (Chan _ nameB _) = nameA `compare` nameB

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
subsituteTypeVar _ _ Unknown = Unknown
subsituteTypeVar free bound (DualOf var) = DualOf (subsituteTypeVar free bound var)

subsituteType :: Text -> Int -> Type -> Type
subsituteType free bound (Var var loc) = Var (subsituteTypeVar free bound var) loc
subsituteType _    _     others        = others

subsituteChannel :: Text -> Int -> Chan -> Chan
subsituteChannel free bound (Chan var name loc)
  | Free free == var = Chan (Bound bound) name loc
  | otherwise        = Chan var name loc

subsituteProcess :: Text -> Int -> Process -> Process
subsituteProcess free bound process = case process of
  Call name loc -> Call name loc
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

-- --------------------------------------------------------------------------------
-- -- | Converting to Abstract Binding Tree
--
-- instance ToAbstract TypeVar A.TypeVar where
--     toAbstract (TypeVar (Free _) name _) = A.Named name
--     toAbstract (TypeVar (Bound index) _ _) = A.Nameless index
--
-- instance ToAbstract TypeName A.TypeName where
--     toAbstract (TypeName name _) = name
--
-- instance ToAbstract Name A.Name where
--   toAbstract (Name name _) = name
--
-- instance ToAbstract Chan A.Chan where
--   toAbstract (Chan var name _) = A.Chan var name
--
-- instance ToAbstract Program A.Program where
--   toAbstract (Program declarations _) =
--     A.Program (map toAbstract declarations)
--
-- instance ToAbstract Declaration A.Declaration where
--     toAbstract (TypeSig name session _) =
--         A.TypeSig (toAbstract name) (toAbstract session)
--     toAbstract (TermDefn name process _) =
--         A.TermDefn (toAbstract name) (toAbstract process)
--
-- instance ToAbstract Session A.Session where
--     toAbstract (Session pairs _) = Map.mapKeys toAbstract $ Map.map toAbstract $ pairs
--
-- instance ToAbstract Process A.Process where
--     toAbstract (Call name _) =
--         A.Call
--             (toAbstract name)
--     toAbstract (Link nameA nameB _) =
--         A.Link
--             (toAbstract nameA)
--             (toAbstract nameB)
--     toAbstract (Compose name Nothing procA procB _) =
--         A.Compose
--             (toAbstract name)
--             Nothing
--             (toAbstract procA)
--             (toAbstract procB)
--     toAbstract (Compose name (Just t) procA procB _) =
--         A.Compose
--             (toAbstract name)
--             (Just (toAbstract t))
--             (toAbstract procA)
--             (toAbstract procB)
--     toAbstract (Output nameA nameB procA procB _) =
--         A.Output
--             (toAbstract nameA)
--             (toAbstract nameB)
--             (toAbstract procA)
--             (toAbstract procB)
--     toAbstract (Input nameA nameB proc _) =
--         A.Input
--             (toAbstract nameA)
--             (toAbstract nameB)
--             (toAbstract proc)
--     toAbstract (SelectL name proc _) =
--         A.SelectL
--             (toAbstract name)
--             (toAbstract proc)
--     toAbstract (SelectR name proc _) =
--         A.SelectR
--             (toAbstract name)
--             (toAbstract proc)
--     toAbstract (Choice name procA procB _) =
--         A.Choice
--             (toAbstract name)
--             (toAbstract procA)
--             (toAbstract procB)
--     toAbstract (Accept nameA nameB proc _) =
--         A.Accept
--             (toAbstract nameA)
--             (toAbstract nameB)
--             (toAbstract proc)
--     toAbstract (Request nameA nameB proc _) =
--         A.Request
--             (toAbstract nameA)
--             (toAbstract nameB)
--             (toAbstract proc)
--     toAbstract (OutputT name typ proc _) =
--         A.OutputT
--             (toAbstract name)
--             (toAbstract typ)
--             (toAbstract proc)
--     toAbstract (InputT name typ proc _) =
--         A.InputT
--             (toAbstract name)
--             (toAbstract typ)
--             (toAbstract proc)
--     toAbstract (EmptyOutput name _) =
--         A.EmptyOutput
--             (toAbstract name)
--     toAbstract (EmptyInput name proc _) =
--         A.EmptyInput
--             (toAbstract name)
--             (toAbstract proc)
--     toAbstract (EmptyChoice name _) =
--         A.EmptyChoice
--             (toAbstract name)
--     toAbstract (End _) =
--         A.End
--     toAbstract (Mix p q _) =
--         A.Mix
--             (toAbstract p)
--             (toAbstract q)
--
-- instance ToAbstract Type A.Type where
--     toAbstract (Var i _) =
--         A.Var
--             (toAbstract i)
--     toAbstract (Dual t _) =
--         A.Dual
--             (toAbstract t)
--     toAbstract (Times t u _) =
--         A.Times
--             (toAbstract t)
--             (toAbstract u)
--     toAbstract (Par t u _) =
--         A.Par
--             (toAbstract t)
--             (toAbstract u)
--     toAbstract (Plus t u _) =
--         A.Plus
--             (toAbstract t)
--             (toAbstract u)
--     toAbstract (With t u _) =
--         A.With
--             (toAbstract t)
--             (toAbstract u)
--     toAbstract (Acc t _) =
--         A.Acc
--             (toAbstract t)
--     toAbstract (Req t _) =
--         A.Req
--             (toAbstract t)
--     toAbstract (Exists x t _) =
--         A.Exists
--             (toAbstract x)
--             (toAbstract t)
--             Nothing
--     toAbstract (Forall x t _) =
--         A.Forall
--             (toAbstract x)
--             (toAbstract t)
--     toAbstract (One _) = A.One
--     toAbstract (Bot _) = A.Bot
--     toAbstract (Zero _) = A.Zero
--     toAbstract (Top _) = A.Top
--
--
-- -- instance ToAbstract TypeVar A.TypeVar where
-- --     toAbstract (TypeVar var _) =
-- --         A.Named var
-- --
-- -- instance ToAbstract TypeName A.TypeName where
-- --     toAbstract (TypeName name    _) = name
-- --
-- -- instance ToAbstract Name A.Name where
-- --   toAbstract (Name name _) = name
-- --
-- -- instance ToAbstract Chan A.Chan where
-- --     toAbstract (Chan name _) = name



--------------------------------------------------------------------------------
-- | Instance of Located

instance Located TypeVar where
  locOf (TypeVar _ _ loc) = loc
  locOf Unknown           = NoLoc
  locOf (DualOf v)        = locOf v

instance Located TypeName where
  locOf (TypeName _ loc) = loc

instance Located Name where
  locOf (Name _ loc) = loc

instance Located Chan where
  locOf (Chan _ _ loc) = loc

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
  locOf (Exists _ _ _ loc) = loc
  locOf (Forall _ _ loc) = loc
  locOf (One loc) = loc
  locOf (Bot loc) = loc
  locOf (Zero loc) = loc
  locOf (Top loc) = loc

--------------------------------------------------------------------------------
-- | Instance of HasDual

instance HasDual TypeVar where
  dual (TypeVar v n l)  = DualOf (TypeVar v n l)
  dual Unknown          = Unknown
  dual (DualOf v)       = v

instance HasDual Type where
  dual (Var a l)        = Var (dual a) l
  dual (Dual a l)       = a
  dual (Times a b l)    = Par (dual a) (dual b) l
  dual (Par a b l)      = Times (dual a) (dual b) l
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
