module Syntax.Abstract where

import Syntax.Base
--
-- import Data.Loc (Loc(..), Located(..))
import Data.Text (Text)
-- import qualified Data.Set as Set
-- import Data.Set (Set)
import Data.Map (Map)
-- import Data.Function (on)

--------------------------------------------------------------------------------
-- | Concrete Binding Tree

-- Names
type Name     = Text
type TypeName = Text

-- Channel
type Chan     = Text

-- Program
type Program = Map Name Process

-- Process
data Process
  = Atom      Name
  | Link      Chan Chan
  | Compose   Chan Process Process
  | Output    Chan Chan Process Process
  | Input     Chan Chan Process
  | SelectL   Chan Process
  | SelectR   Chan Process
  | Choice    Chan Process Process
  | Accept    Chan Chan Process
  | Request   Chan Chan Process
  | OutputT   Chan Process
  | InputT    Chan Process
  | EmptyOutput Chan
  | EmptyInput  Chan Process
  | EmptyChoice Chan
  | End
  | Mix       Process   Process
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Instances



--------------------------------------------------------------------------------
-- | Make free variables bounded

subsituteChannel :: Chan -> Chan -> Chan -> Chan
subsituteChannel old new chan = if chan == old then new else old

subsitute :: Chan -> Chan -> Process -> Process
subsitute old new process = case process of
  Atom name -> Atom name
  Link x y -> Link (chan x) (chan y)
  Compose x a b -> Compose (chan x) (subst a) (subst b)
  Output x y a b -> Output (chan x) (chan y) (subst a) (subst b)
  Input x y a -> Input (chan x) (chan y) (subst a)
  SelectL x a -> SelectL (chan x) (subst a)
  SelectR x a -> SelectR (chan x) (subst a)
  Choice x a b -> Choice (chan x) (subst a) (subst b)
  Accept x y a -> Accept (chan x) (chan y) (subst a)
  Request x y a -> Request (chan x) (chan y) (subst a)
  OutputT x a -> OutputT (chan x) (subst a)
  InputT x a -> InputT (chan x) (subst a)
  EmptyOutput x -> EmptyOutput (chan x)
  EmptyInput x a -> EmptyInput (chan x) (subst a)
  EmptyChoice x -> EmptyChoice (chan x)
  End -> End
  Mix p q -> Mix (subst p) (subst q)
  where
    chan = subsituteChannel old new
    subst = subsitute old new


--------------------------------------------------------------------------------
-- Free variables

-- freeChans :: Process -> FreeChans
-- freeChans (Process p xs _) = xs
--   -- where
--   --
--   --
--   --   case p of
-- freeChans' :: Proc -> FreeChans
-- freeChans' p = case p of
--   Atom _ xs -> xs
--   Link x y -> Set.fromList [chanName x, chanName y]
--   Compose x _ p q -> Set.delete (chanName x) $ Set.union (freeChans p) (freeChans q)
--   Output x y p q -> Set.insert (chanName x) $ Set.delete (chanName y) $ Set.union (freeChans p) (freeChans q)
--   Input x y p -> Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
--   SelectL x p -> Set.insert (chanName x) $ freeChans p
--   SelectR x p -> Set.insert (chanName x) $ freeChans p
--   Choice x p q -> Set.insert (chanName x) $ Set.union (freeChans p) (freeChans q)
--   Accept x y p ->Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
--   Request x y p -> Set.insert (chanName x) $ Set.delete (chanName y) (freeChans p)
--   OutputT x _ p -> Set.insert (chanName x) (freeChans p)
--   InputT x _ p -> Set.insert (chanName x) (freeChans p)
--   EmptyOutput x -> Set.singleton (chanName x)
--   EmptyInput x p -> Set.insert (chanName x) (freeChans p)
--   EmptyChoice x -> Set.singleton (chanName x)
--   End -> Set.empty
--   Mix p q -> Set.union (freeChans p) (freeChans q)
