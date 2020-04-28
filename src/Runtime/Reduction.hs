module Runtime.Reduction
  ( findMatchingChannels
  , headChan
  , Match(..)
  )
where

    -- import Syntax.Binding
import           Syntax.Abstract

import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

-- import           Debug.Trace
-- import           Pretty

data Match = MatchingPair Chan Int Int | MatchingLink Chan Int
    deriving (Show, Eq, Ord)

-- The depth of each channel in a process
type Distances = Map Chan Int

findMatchingChannels :: Process -> Set Match
findMatchingChannels = fst . find
 where
  -- increase the distance of every entry in the Map
  incr :: Distances -> Distances
  incr = fmap succ

  -- matchLink :: Chan -> Distances -> Set Match
  -- matchLink chan distances = case Map.lookup chan distances of
  --   Nothing -> Set.empty
  --   Just n  -> Set.singleton (MatchingLink chan n)

  -- merge two Distance Map into one Set of Matches
  matchPair :: Distances -> Distances -> Set Match
  matchPair a b =
    Map.foldrWithKey (\k (m, n) -> Set.insert (MatchingPair k m n)) Set.empty
      $ Map.intersectionWith (,) a b
  --
  find :: Process -> (Set Match, Distances)
  find (Compose chan p q) =
    let
      (pms, phs)   = find p
      (qms, qhs)   = find q

      matchedLinks = case (p, q) of
        (Link _ x, Link _ y) -> if chan == x || chan == y
          then Set.singleton (MatchingLink chan 0)
          else Set.empty
        (Link _ x, _) ->
          if chan == x then Set.singleton (MatchingLink chan 0) else Set.empty
        (_, Link _ y) ->
          if chan == y then Set.singleton (MatchingLink chan 0) else Set.empty
        _ -> Set.empty
      -- NOTE: if `match phs qhs` returns something, the corresponding channel will have to be `chan`
      matchedPairs = matchPair phs qhs
      -- merge both Distance mappings of `p` and `q`
      -- but remove the free channel `chan` because it will be closed by Compose
      hs           = Map.delete chan (phs `Map.union` qhs)
        -- bump the distance in `hs` by 1
    in
      (matchedLinks <> matchedPairs <> pms <> qms, incr hs)
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
  | LNK       -- link
  deriving (Show, Ord, Eq)

toHead :: Process -> Head
toHead (Output n _ _ _) = Reactive O n
toHead (Input n _ _   ) = Reactive I n
toHead (SelectL n _   ) = Reactive SE n
toHead (SelectR n _   ) = Reactive SE n
toHead (Choice  n _ _ ) = Reactive CH n
toHead (Accept  n _ _ ) = Reactive AC n
toHead (Request n _ _ ) = Reactive RQ n
toHead (OutputT n _   ) = Reactive TO n
toHead (InputT  n _   ) = Reactive TI n
toHead (EmptyOutput n ) = Reactive EO n
toHead (EmptyInput n _) = Reactive EI n
toHead (Link       _ n) = Reactive LNK n
toHead _                = Inert
