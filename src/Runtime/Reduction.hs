module Runtime.Reduction
  ( findMatches
  , headChan
  , Match(..)
  ) where

    -- import Syntax.Binding
import Syntax.Abstract

import Data.Text (Text)
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace
import Pretty


data Match = Match Chan Int Int
    deriving (Show, Eq, Ord)
type Distances = Map Chan Int

findMatches :: Process -> Set Match
findMatches = fst . find
  where
    -- increase the distance of every entry in the Map
    incr :: Distances -> Distances
    incr = fmap succ

    -- merge two Distance Map into one Set of Matches
    match :: Distances -> Distances -> Set Match
    match a b = Map.foldrWithKey (\k (m, n) -> Set.insert (Match k m n)) Set.empty $ Map.intersectionWith (,) a b
    --
    find :: Process -> (Set Match, Distances)
    find (Compose n p q) =
      let (pms, phs) = find p
          (qms, qhs) = find q
          ms         = match phs qhs <> pms <> qms
          hs         = Map.delete n (phs `Map.union` qhs)
      in  (ms, incr hs)
    find others = case toHead others of
      Inert           -> (Set.empty, Map.empty)
      Reactive _ chan -> (Set.empty, Map.singleton chan 0)


headChan :: Process -> Maybe Chan
headChan process = case toHead process of
   (Reactive _ n) -> Just n
   _              -> Nothing

--------------------------------------------------------------------------------
-- | Head & Kinds

data Head
  = Reactive Kind Chan
  | Inert
  deriving (Show, Ord, Eq)

data Kind
  = I  | O    -- input/output
  | TI | TO   -- type input/output
  | EI | EO   -- empty input/output
  | SE | CH   -- choice/select
  | AC | RQ   -- accept/request
  deriving (Show, Ord, Eq)

-- invert :: Kind -> Kind
-- invert I = O
-- invert O = I
-- invert SE = CH
-- invert CH = SE
-- invert AC = RQ
-- invert RQ = AC
-- invert TI = TO
-- invert TO = TI
-- invert EO = EI
-- invert EI = EO
--
-- invertHead :: Head -> Head
-- invertHead (Reactive k n) = Reactive (invert k) n
-- invertHead others = others
--
-- reducible :: Head -> Head -> Bool
-- reducible (Reactive k m) (Reactive l n) = k == invert l &&  m == n
-- reducible _ _ = False

toHead :: Process -> Head
toHead (Output n _ _ _) = Reactive O n
toHead (Input n _ _) = Reactive I n
toHead (SelectL n _) = Reactive SE n
toHead (SelectR n _) = Reactive SE n
toHead (Choice n _ _) = Reactive CH n
toHead (Accept n _ _) = Reactive AC n
toHead (Request n _ _) = Reactive RQ n
toHead (OutputT n _) = Reactive TO n
toHead (InputT n _) = Reactive TI n
toHead (EmptyOutput n) = Reactive EO n
toHead (EmptyInput n _) = Reactive EI n
toHead _ = Inert
