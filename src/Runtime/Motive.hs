module Runtime.Motive
  ( findMotive
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

-- redexLevel :: Process -> Maybe Int
-- redexLevel process = fmap (redexLevel' (toProc process)) motiveChannel
--   where
--     motiveChannel :: Maybe Text
--     motiveChannel = findMotive process
--
--     redexLevel' :: Proc -> Text -> Int
--     redexLevel' () = undefined

-- find the next channel where the reduction's gonna happen
findMotive :: Process -> Maybe Text
findMotive process = evalState (findMotive' $ toProc process) Set.empty
  where
    lookupOrInsert :: Chan -> Motive -> Motive -> MotiveM (Maybe Text)
    lookupOrInsert chan target self = do
      let name = chanName chan
      -- see if the (name, target) pair is present in the set
      -- insert (name, self) if not
      set <- get
      if Set.member (name, target) set
        then return $ Just name
        else do
          modify (Set.insert (name, self))
          return Nothing

    findMotive' :: Proc -> MotiveM (Maybe Text)
    -- findMotive' (Call _ (Right (Process p _ _))) = findMotive' p
    findMotive' (Compose _ _ p q) = do
      results <- catMaybes <$> mapM findMotive' (map toProc [p, q])
      case length results of
        0 -> return $ Nothing
        _ -> return $ Just $ head results
    findMotive' (Output n _ _ _) = lookupOrInsert n I O
    findMotive' (Input n _ _) = lookupOrInsert n O I
    findMotive' (SelectL n _) = lookupOrInsert n CH SE
    findMotive' (SelectR n _) = lookupOrInsert n CH SE
    findMotive' (Choice n _ _) = lookupOrInsert n SE CH
    findMotive' (Accept n _ _) = lookupOrInsert n RQ AC
    findMotive' (Request n _ _) = lookupOrInsert n AC RQ
    findMotive' (OutputT n _ _) = lookupOrInsert n TI TO
    findMotive' (InputT n _ _) = lookupOrInsert n TO TI
    findMotive' (EmptyOutput n) = lookupOrInsert n EI EO
    findMotive' (EmptyInput n _) = lookupOrInsert n EO EI
    findMotive' _ = return Nothing


data Motive = I  | O    -- input/output
            | TI | TO   -- type input/output
            | EI | EO   -- empty input/output
            | SE | CH   -- choice/select
            | AC | RQ   -- accept/request
            deriving (Show, Ord, Eq)

-- toMotive :: Proc -> Maybe Motive
-- toMotive (Output _ _ _ _) = Just O
-- toMotive (Input _ _ _) = Just I
-- toMotive (SelectL _ _) = Just SE
-- toMotive (SelectR _ _) = Just SE
-- toMotive (Choice _ _ _) = Just CH
-- toMotive (Accept _ _ _) = Just AC
-- toMotive (Request _ _ _) = Just RQ
-- toMotive (OutputT _ _ _) = Just TO
-- toMotive (InputT _ _ _) = Just TI
-- toMotive (EmptyOutput _) = Just EO
-- toMotive (EmptyOutput _) = Just EO
-- toMotive (EmptyInput _ _) = Just EI
-- toMotive _ = Nothing

type MotiveM = State (Set (Text, Motive))
