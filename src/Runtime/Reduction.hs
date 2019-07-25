module Runtime.Reduction
  ( findMatches
  -- , Match(..)
  ) where

import Syntax.Binding

import Data.Text (Text)
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

-- import Debug.Trace
-- import Pretty


-- data Match = Match Chan Int Int
--     deriving (Show)
type Distances = Map Chan Int


-- creep (Match chan i j) (Compose n _ (Compose ) q) =

findMatches :: Process -> Map Chan (Int, Int)
findMatches = fst . find . toProc
  where
    -- increase the distance of every entry in the Map
    incr :: Distances -> Distances
    incr = fmap succ
    -- merge two Distance Map into one Map of matched channels
    match :: Distances -> Distances -> Map Chan (Int, Int)
    match = Map.intersectionWith (,)
    --
    find :: Proc -> (Map Chan (Int, Int), Distances)
    find (Compose n _ p q) =
      let (pms, phs) = find $ toProc p
          (qms, qhs) = find $ toProc q
          ms         = match phs qhs <> pms <> qms
          hs         = Map.delete n (phs `Map.union` qhs)
      in  (ms, incr hs)
    find others = case toHead others of
      Inert           -> (Map.empty, Map.empty)
      Reactive _ chan -> (Map.empty, Map.singleton chan 0)

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

-- headChan :: Head -> Maybe Chan
-- headChan (Reactive _ n) = Just n
-- headChan _ = Nothing
--
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

toHead :: Proc -> Head
toHead (Output n _ _ _) = Reactive O n
toHead (Input n _ _) = Reactive I n
toHead (SelectL n _) = Reactive SE n
toHead (SelectR n _) = Reactive SE n
toHead (Choice n _ _) = Reactive CH n
toHead (Accept n _ _) = Reactive AC n
toHead (Request n _ _) = Reactive RQ n
toHead (OutputT n _ _) = Reactive TO n
toHead (InputT n _ _) = Reactive TI n
toHead (EmptyOutput n) = Reactive EO n
toHead (EmptyInput n _) = Reactive EI n
toHead _ = Inert
