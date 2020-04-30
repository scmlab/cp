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

data Match
    = MatchingPair Chan Int Int
    | MatchingLinkLeft Chan Int
    | MatchingLinkRight Chan Int
    deriving (Show, Eq)

instance Ord Match where
  compare (MatchingPair _ m n) (MatchingPair _ o p) = compare (m + n) (o + p)
  compare (MatchingPair _ m n   ) (MatchingLinkLeft  _ o) = compare (m + n) o
  compare (MatchingPair _ m n   ) (MatchingLinkRight _ o) = compare (m + n) o
  compare (MatchingLinkLeft  _ o) (MatchingPair _ m n   ) = compare o (m + n)
  compare (MatchingLinkRight _ o) (MatchingPair _ m n   ) = compare o (m + n)
  compare (MatchingLinkLeft  _ m) (MatchingLinkLeft  _ n) = compare m n
  compare (MatchingLinkLeft  _ _) (MatchingLinkRight _ _) = LT
  compare (MatchingLinkRight _ m) (MatchingLinkRight _ n) = compare m n
  compare (MatchingLinkRight _ _) (MatchingLinkLeft  _ _) = GT

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
  -- matchPair :: Distances -> Distances -> Set Match
  -- matchPair a b =
  --   Map.foldrWithKey (\k (m, n) -> Set.insert (MatchingPair k m n)) Set.empty
  --     $ Map.intersectionWith (,) a b
  --
  find :: Process -> (Set Match, (Distances, Distances, Distances, Distances))
  find (Compose chan p q) =
    let
      (pms, (p1, p2, pAC, pRQ)) = find p
      (qms, (q1, q2, qAC, qRQ)) = find q

      -- see if are 2 univalent terms ready to be bound together
      matchedPairs = case (Map.lookup chan p1, Map.lookup chan q1) of
        (Just m, Just n) -> Set.singleton (MatchingPair chan m n)
        _                -> Set.empty

      matchedPLinks = case Map.lookup chan p2 of
        Just m  -> Set.singleton (MatchingLinkLeft chan m)
        Nothing -> Set.empty
      matchedQLinks = case Map.lookup chan q2 of
        Just n  -> Set.singleton (MatchingLinkRight chan n)
        Nothing -> Set.empty

      matchedPACRQ = case (Map.lookup chan pAC, Map.lookup chan qRQ) of
        (Just m, Just n) -> Set.singleton (MatchingPair chan m n)
        _                -> Set.empty

      matchedQACRQ = case (Map.lookup chan qAC, Map.lookup chan pRQ) of
        (Just m, Just n) -> Set.singleton (MatchingPair chan n m)
        _                -> Set.empty

      matched =
        matchedPairs
          <> matchedPLinks
          <> matchedQLinks
          <> matchedPACRQ
          <> matchedQACRQ

      -- merge both Distance mappings of `p` and `q`
      -- but remove the free channel `chan` because it will be closed by Compose
      pq1  = Map.delete chan (p1 <> q1)
      pq2  = p2 <> q2
      pqAC = pAC <> qAC
      pqRQ = pRQ <> qRQ
        -- bump the distances by 1
    in
      (matched <> pms <> qms, (incr pq1, incr pq2, incr pqAC, incr pqRQ))
  find others = case toValence others of
    Nonvalent -> (Set.empty, (Map.empty, Map.empty, Map.empty, Map.empty))
    Univalent AC chan ->
      (Set.empty, (Map.empty, Map.empty, Map.singleton chan 0, Map.empty))
    Univalent RQ chan ->
      (Set.empty, (Map.empty, Map.empty, Map.empty, Map.singleton chan 0))
    Univalent _ chan ->
      (Set.empty, (Map.singleton chan 0, Map.empty, Map.empty, Map.empty))
    Bivalent x y ->
      ( Set.empty
      , (Map.empty, Map.fromList [(x, 0), (y, 0)], Map.empty, Map.empty)
      )

headChan :: Process -> Maybe Chan
headChan process = case toValence process of
  (Univalent _ n) -> Just n
  _               -> Nothing

--------------------------------------------------------------------------------
-- | Head & Kinds

data Valence
  = Nonvalent
  | Univalent Kind Chan
  | Bivalent Chan Chan
  deriving (Show, Ord, Eq)

data Kind
  = I  | O          -- input/output
  | TI | TO         -- type input/output
  | EI | EO         -- empty input/output
  | SE | CH         -- choice/select
  | AC | RQ         -- accept/request
  deriving (Show, Ord, Eq)

toValence :: Process -> Valence
toValence (Output n _ _ _) = Univalent O n
toValence (Input n _ _   ) = Univalent I n
toValence (SelectL n _   ) = Univalent SE n
toValence (SelectR n _   ) = Univalent SE n
toValence (Choice  n _ _ ) = Univalent CH n
toValence (Accept  n _ _ ) = Univalent AC n
toValence (Request n _ _ ) = Univalent RQ n
toValence (OutputT n _   ) = Univalent TO n
toValence (InputT  n _   ) = Univalent TI n
toValence (EmptyOutput n ) = Univalent EO n
toValence (EmptyInput n _) = Univalent EI n
toValence (Link       m n) = Bivalent m n
toValence _                = Nonvalent
