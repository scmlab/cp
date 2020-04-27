
module Syntax.Concrete where

import           Syntax.Base

import           Data.Loc                       ( Loc(..)
                                                , Located(..)
                                                )
import           Data.Text                      ( Text )
import           Data.List                      ( elemIndex )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set
                                                , (\\)
                                                )
import qualified Data.Set                      as Set
import           Data.Maybe                     ( mapMaybe )
-- import Prelude
import           Data.Function                  ( on )

import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Reader

import           Debug.Trace


--------------------------------------------------------------------------------
-- | Concrete Syntax Tree

-- variables and names
data Name     = Name      Text Loc deriving (Show)
data Chan     = Chan      Text Loc deriving (Show)
data TypeName = TypeName  Text Loc deriving (Show)
data TypeVar  = TypeVar   Text Loc
              | Unknown
              deriving (Show)

chanName :: Chan -> Text
chanName (Chan name _) = name

data Program = Program [Declaration] Loc deriving (Show)

data Definition = Paired   Name Process Session
                | TypeOnly Name Session
                | TermOnly Name Process
                deriving (Show)

type Definitions = Map Name Definition

data Declaration = TypeSig  Name SessionSyntax Loc
                 | TermDefn Name Process Loc
                 deriving (Show)

type Session = Map Chan Type
data SessionSyntax = SessionSyntax Session Loc deriving (Show)

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
          | Exists
              TypeVar   -- X: the type variable to be substituted
              Type      -- B: the type (variable) representing the type before substitution
              (Maybe (Type, Type)) -- extra information, useful when composing with Forall
                 -- fst: the type to be substituted with
                 -- snd: the resulting type after substitution
                 -- such that   snd = B {fst / X}
              Loc
          | Forall  TypeVar   Type  Loc
          | One                     Loc
          | Bot                     Loc
          | Zero                    Loc
          | Top                     Loc
          deriving (Show)

instance HasDual Type where
  dual (Var  i l      ) = Dual (Var i l) l
  dual (Dual a _      ) = a
  dual (Times a b l   ) = Par (dual a) (dual b) l
  dual (Par   a b l   ) = Times (dual a) (dual b) l
  dual (Plus  a b l   ) = With (dual a) (dual b) l
  dual (With  a b l   ) = Plus (dual a) (dual b) l
  dual (Acc a l       ) = Req (dual a) l
  dual (Req a l       ) = Acc (dual a) l
  dual (Exists x a _ l) = Forall x (dual a) l
  dual (Forall x a l  ) = Exists x (dual a) Nothing l
  dual (One  l        ) = Bot l
  dual (Bot  l        ) = One l
  dual (Zero l        ) = Top l
  dual (Top  l        ) = Zero l

--------------------------------------------------------------------------------
-- | Instances

instance Eq Type where
  (==) = eq `on` dual
   where
    eq :: Type -> Type -> Bool
    eq (Var  a _      ) (Var  b _      ) = a == b
    eq (Dual a _      ) (Dual b _      ) = eq a b
    eq (Times a b _   ) (Times c d _   ) = eq a c && eq b d
    eq (Par   a b _   ) (Par   c d _   ) = eq a c && eq b d
    eq (Plus  a b _   ) (Plus  c d _   ) = eq a c && eq b d
    eq (With  a b _   ) (With  c d _   ) = eq a c && eq b d
    eq (Acc a _       ) (Acc b _       ) = eq a b
    eq (Req a _       ) (Req b _       ) = eq a b
    eq (Exists a b c _) (Exists d e f _) = a == d && eq b e && c == f
    eq (Forall a b _  ) (Forall c d _  ) = a == c && eq b d
    eq (One  _        ) (One  _        ) = True
    eq (Bot  _        ) (Bot  _        ) = True
    eq (Zero _        ) (Zero _        ) = True
    eq (Top  _        ) (Top  _        ) = True
    eq _                _                = False

instance Ord Type where
  compare _ _ = EQ

instance Eq TypeVar where
    -- Unknown is equivalent to anything
  Unknown     == _           = True
  _           == Unknown     = True
  --
  TypeVar i _ == TypeVar j _ = i == j
  _           == _           = False

instance Ord TypeVar where
  Unknown     `compare` Unknown     = EQ
  Unknown     `compare` _           = LT
  _           `compare` Unknown     = GT
  TypeVar i _ `compare` TypeVar j _ = i `compare` j


instance Eq Name where
  (Name a _) == (Name b _) = a == b

instance Ord Name where
  (Name a _) `compare` (Name b _) = a `compare` b

instance Eq Chan where
  (==) = (==) `on` chanName

instance Ord Chan where
  compare = compare `on` chanName

--------------------------------------------------------------------------------
-- | Helper functions

typeSigName :: Declaration -> Maybe Name
typeSigName (TypeSig n _ _) = Just n
typeSigName _               = Nothing

termDefnName :: Declaration -> Maybe Name
termDefnName (TermDefn n _ _) = Just n
termDefnName _                = Nothing

toProcess :: Definition -> Maybe Process
toProcess (Paired _ term _) = Just term
toProcess (TypeOnly _ _   ) = Nothing
toProcess (TermOnly _ term) = Just term

sessionKeys :: Session -> Set Text
sessionKeys = Set.fromList . map (\(Chan n _) -> n) . Map.keys

convert :: SessionSyntax -> Session
convert (SessionSyntax xs _) = xs

insertSessionSyntax :: Chan -> Type -> SessionSyntax -> SessionSyntax
insertSessionSyntax x t (SessionSyntax pairs m) =
  SessionSyntax (Map.insert x t pairs) m

emptySessionSyntax :: Loc -> SessionSyntax
emptySessionSyntax l = SessionSyntax Map.empty l

singletonSessionSyntax :: Chan -> Type -> Loc -> SessionSyntax
singletonSessionSyntax x t l = SessionSyntax (Map.insert x t Map.empty) l

toDefinitions :: Program -> Definitions
toDefinitions (Program declarations _) = definitions
 where
  toTypeSigPair (TypeSig n s _) = Just (n, convert s)
  toTypeSigPair _               = Nothing

  toTermDefnPair (TermDefn n t _) = Just (n, t)
  toTermDefnPair _                = Nothing

  typeSigs :: Map Name Session
  typeSigs = Map.fromList $ mapMaybe toTypeSigPair declarations

  termDefns :: Map Name Process
  termDefns = Map.fromList $ mapMaybe toTermDefnPair declarations

  paired :: Definitions
  paired = Map.mapWithKey (\n (t, s) -> Paired n t s)
    $ Map.intersectionWith (,) termDefns typeSigs

  termsOnly :: Definitions
  termsOnly =
    Map.mapWithKey (\n t -> TermOnly n t) $ Map.difference termDefns typeSigs

  typesOnly :: Definitions
  typesOnly =
    Map.mapWithKey (\n s -> TypeOnly n s) $ Map.difference typeSigs termDefns

  definitions :: Definitions
  definitions = Map.union paired $ Map.union termsOnly typesOnly

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
  locOf (TypeSig  _ _ loc) = loc
  locOf (TermDefn _ _ loc) = loc

instance Located Process where
  locOf (Call _ loc         ) = loc
  locOf (Link _ _ loc       ) = loc
  locOf (Compose _ _ _ _ loc) = loc
  locOf (Output  _ _ _ _ loc) = loc
  locOf (Input _ _ _ loc    ) = loc
  locOf (SelectL _ _ loc    ) = loc
  locOf (SelectR _ _ loc    ) = loc
  locOf (Choice  _ _ _ loc  ) = loc
  locOf (Accept  _ _ _ loc  ) = loc
  locOf (Request _ _ _ loc  ) = loc
  locOf (OutputT _ _ _ loc  ) = loc
  locOf (InputT  _ _ _ loc  ) = loc
  locOf (EmptyOutput _ loc  ) = loc
  locOf (EmptyInput _ _ loc ) = loc
  locOf (EmptyChoice _ loc  ) = loc
  locOf (End loc            ) = loc
  locOf (Mix _ _ loc        ) = loc

instance Located Type where
  locOf (Var  _ loc      ) = loc
  locOf (Dual _ loc      ) = loc
  locOf (Times _ _ loc   ) = loc
  locOf (Par   _ _ loc   ) = loc
  locOf (Plus  _ _ loc   ) = loc
  locOf (With  _ _ loc   ) = loc
  locOf (Acc _ loc       ) = loc
  locOf (Req _ loc       ) = loc
  locOf (Exists _ _ _ loc) = loc
  locOf (Forall _ _ loc  ) = loc
  locOf (One  loc        ) = loc
  locOf (Bot  loc        ) = loc
  locOf (Zero loc        ) = loc
  locOf (Top  loc        ) = loc



--------------------------------------------------------------------------------
-- Call graph

type CallGraph = Map Name (Set Name)
type CallM = State CallGraph

buildCallGraph :: Definitions -> CallGraph
buildCallGraph definitions = execState (buildAll definitions) initCallGraph
 where
  -- top level names paired with an empty set
  initCallGraph :: CallGraph
  initCallGraph = Map.map (const Set.empty) definitions

  buildAll :: Definitions -> CallM ()
  buildAll = mapM_ (uncurry build) . Map.toList . Map.mapMaybe toProcess

  insert :: Name -> Name -> CallM ()
  insert a b = modify (Map.insertWith Set.union a (Set.singleton b))

  build :: Name -> Process -> CallM ()
  build name process = case process of
    Call callee _     -> insert name callee
    Link _ _ _        -> return ()
    Compose _ _ p q _ -> do
      build name p
      build name q
    Output _ _ p q _ -> do
      build name p
      build name q
    Input _ _ p _  -> build name p
    SelectL _ p _  -> build name p
    SelectR _ p _  -> build name p
    Choice _ p q _ -> do
      build name p
      build name q
    Accept  _ _ p _  -> build name p
    Request _ _ p _  -> build name p
    OutputT _ _ p _  -> build name p
    InputT  _ _ p _  -> build name p
    EmptyOutput _ _  -> return ()
    EmptyInput _ p _ -> build name p
    EmptyChoice _ _  -> return ()
    End _            -> return ()
    Mix p q _        -> do
      build name p
      build name q

-- represents the order of calls (from back to front)
type Path = [Name]
type LoopM = ExceptT Path (Reader CallGraph)

-- extending the Path by one 
step :: Path -> LoopM [Path]
step input = do
  graph <- ask
  paths <- case input of
    -- constructing initial paths with the entries
    []                    -> return [ [p] | p <- Map.keys graph ]
    (current : traversed) -> do
      -- throw error when the newly added name is already traversed
      case elemIndex current traversed of
        Nothing -> return ()
        -- `current` occurs at the nths position of `traversed`, cut that cycling part down and report it
        Just n  -> throwError (current : take n traversed)

      case Map.lookup current graph of
        -- `current` not present in the graph, end it here
        Nothing -> return []
        -- `current` leads to some more names
        Just ns -> return [ (n : traversed) | n <- Set.toList ns ]

  results <- mapM step paths
  return (concat results)

detectLoop :: CallGraph -> Maybe Path
detectLoop graph = case runReader (runExceptT (step [])) graph of
  Left  loop -> Just loop
  Right _    -> Nothing

detectOutOfScope :: CallGraph -> Maybe Name
detectOutOfScope graph = Set.lookupMin badCalls
 where
  topLevelBindings :: Set Name
  topLevelBindings = Map.keysSet graph

  calls :: Set Name
  calls = Set.unions (Map.elems graph)

  badCalls :: Set Name
  badCalls = calls \\ topLevelBindings

--------------------------------------------------------------------------------
-- Free variables

--
-- freeChans :: Process -> FreeChans
-- freeChans p = case p of
--   Call _ xs _ -> xs
--   Link x y _ -> Set.fromList [chanName x, chanName y]
--   Compose x _ p q _ -> Set.delete (chanName x) $ Set.union (freeChans p) (freeChans q)
--   Output x y p q _ -> Set.insert (chanName x) $ Set.delete (chanName y) $ Set.union (freeChans p) (freeChans q)
--   Input x y p _ -> Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
--   SelectL x p _ -> Set.insert (chanName x) $ freeChans p
--   SelectR x p _ -> Set.insert (chanName x) $ freeChans p
--   Choice x p q _ -> Set.insert (chanName x) $ Set.union (freeChans p) (freeChans q)
--   Accept x y p _ -> Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
--   Request x y p _ -> Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
--   OutputT x _ p _ -> Set.insert (chanName x) (freeChans p)
--   InputT x _ p _ -> Set.insert (chanName x) (freeChans p)
--   EmptyOutput x _ -> Set.singleton (chanName x)
--   EmptyInput x p _ -> Set.insert (chanName x) (freeChans p)
--   EmptyChoice x _ -> Set.singleton (chanName x)
--   End _ -> Set.empty
--   Mix p q _ -> Set.union (freeChans p) (freeChans q)
