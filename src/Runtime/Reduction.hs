module Runtime.Reduction
  ( findRedexChan
  ) where

import Syntax.Binding

import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except

-- import Debug.Trace
-- import Pretty

-- -- A redex is considered "settled", if it is located at the bottom most of a tree
-- redexSettled :: Process -> Bool
-- redexSettled process = fmap (redexSettled' (toProc process)) redexChan
--   where
--     redexChan :: Maybe Chan
--     redexChan = findRedexChan process
--
--     redexSettled' :: Proc -> Bool
--     redexSettled' (Compose n _ p q) =

-- redexLevel :: Process -> Maybe Int
-- redexLevel process = fmap (redexLevel' (toProc process)) motiveChannel
--   where
--     motiveChannel :: Maybe Text
--     motiveChannel = findRedexChan process
--
--     redexLevel' :: Proc -> Text -> Int
--     redexLevel' (Atom _ _) = undefined

-- find the next channel where the reduction's gonna happen
findRedexChan :: Process -> Maybe Chan
findRedexChan process = evalState (findRedexChan' $ toProc process) Set.empty
  where
    match :: Proc -> MotiveM (Maybe Chan)
    match proc = case toHead proc of
      Inert -> return Nothing
      redex@(Active _ chan) -> do
        set <- get
        if Set.member (invert redex) set
          then return $ Just chan
          else do
            modify (Set.insert redex)
            return Nothing


    findRedexChan' :: Proc -> MotiveM (Maybe Chan)
    findRedexChan' (Compose _ _ p q) = do
      results <- catMaybes <$> mapM findRedexChan' (map toProc [p, q])
      case length results of
        0 -> return $ Nothing
        _ -> return $ Just $ head results
    findRedexChan' p = match p

data Head
  = Active Kind Chan
  | Inert
  deriving (Show, Ord, Eq)
data Kind
      = I  | O    -- input/output
      | TI | TO   -- type input/output
      | EI | EO   -- empty input/output
      | SE | CH   -- choice/select
      | AC | RQ   -- accept/request
      deriving (Show, Ord, Eq)

invert :: Head -> Head
invert (Active I n) = Active O n
invert (Active O n) = Active I n
invert (Active SE n) = Active CH n
invert (Active CH n) = Active SE n
invert (Active AC n) = Active RQ n
invert (Active RQ n) = Active AC n
invert (Active TI n) = Active TO n
invert (Active TO n) = Active TI n
invert (Active EO n) = Active EI n
invert (Active EI n) = Active EO n

reducible :: Head -> Head -> Bool
reducible (Active O m) (Active I n) = m == n
reducible (Active I m) (Active O n) = m == n
reducible (Active SE m) (Active CH n) = m == n
reducible (Active CH m) (Active SE n) = m == n
reducible (Active AC m) (Active RQ n) = m == n
reducible (Active RQ m) (Active AC n) = m == n
reducible (Active TO m) (Active TI n) = m == n
reducible (Active TI m) (Active TO n) = m == n
reducible (Active EO m) (Active EI n) = m == n
reducible (Active EI m) (Active EO n) = m == n
reducible _ _ = False

toHead :: Proc -> Head
toHead (Output n _ _ _) = Active O n
toHead (Input n _ _) = Active I n
toHead (SelectL n _) = Active SE n
toHead (SelectR n _) = Active SE n
toHead (Choice n _ _) = Active CH n
toHead (Accept n _ _) = Active AC n
toHead (Request n _ _) = Active RQ n
toHead (OutputT n _ _) = Active TO n
toHead (InputT n _ _) = Active TI n
toHead (EmptyOutput n) = Active EO n
toHead (EmptyInput n _) = Active EI n
toHead _ = Inert

type MotiveM = State (Set Head)
