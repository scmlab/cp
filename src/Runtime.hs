{-# LANGUAGE OverloadedStrings, DeriveAnyClass                  #-}

module Runtime (evaluate) where


  -- import Syntax.Concrete
import qualified Syntax.Concrete as C
import Syntax.Abstract
import qualified TypeChecking.Unification as U
-- import Pretty

-- import TypeChecking.Base
import Base
import Runtime.Reduction

import Data.Loc (Loc(..))
import Data.Maybe (fromJust)
import qualified Data.List as List
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad.Except

import Pretty

evaluate :: C.Process -> M Process
evaluate process' = do
  definitions <- gets replDefinitions
  process <- abstract (Just definitions) process'
  run process

run :: Process -> M Process
run input = do

  -- run !!
  (result, usedRule) <- lift $ runReaderT (runStateT (runExceptT reduce) Nothing) input

  case result of
    Left e -> throwError $ RuntimeError e
    Right output -> do
      case usedRule of
        Nothing -> return output  -- stuck
        Just rule -> do
          printStatus output rule
          run output     -- keep reducing

printStatus :: Process -> Rule -> M ()
printStatus output rule = do
  liftIO $ putDoc $ line
  liftIO $ putDoc $ paint $ pretty ("=>" :: String) <+> pretty rule <> line
  liftIO $ putDoc $ line
    <> pretty (output)
    <> line
  where
    paint :: Doc AnsiStyle -> Doc AnsiStyle
    paint = annotate (colorDull Blue)

stuck :: RuntimeM Process
stuck = ask

swap :: RuntimeM Process
swap = do
  putRule Swap
  input <- ask
  case input of
    Compose chan p q -> return $ Compose chan q p
    others -> return others

use :: Rule -> Process -> RuntimeM Process
use rule input = do
  p <- hasReduced
  if p
    then stuck
    else do
      putRule rule
      return input

reduce :: RuntimeM Process
reduce = do
  input <- ask
  return input
  -- case toProcess input of
    -- Compose chan _ p q -> do
  --
  --     let matches = findMatches input
  --     if Map.empty matches
  --       then return $ Process input undefined NoLoc
  --       else do
  --         output <- case Map.lookup chan matches of
  --           Nothing     -> do
  --             reduce'
  --           Just (0, 0) -> reduceProcess chan (toProcess p) (toProcess q)
  --           Just (0, n) -> stuck
  --           Just (m, n) -> stuck
  --         return $ Process output undefined NoLoc
  --   _ -> return input
  --
  -- where
  --   reduce' input@(Process (Compose chan _ p q) _ _) = do
  --     let matches = findMatches input
  --     if Map.empty matches
  --       then return input
  --       else do
  --         output <- case Map.lookup chan matches of
  --           Nothing     -> undefined
  --
  --           Just (0, 0) -> reduceProcess chan (toProcess p) (toProcess q)
  --           Just (0, n) -> stuck
  --           Just (m, n) -> stuck
  --         return $ Process output undefined NoLoc
  --   reduce' others = return others


checkChannels :: [Chan] -> RuntimeM ()
checkChannels channels = do
  let groups = map head $ List.group channels
  let allTheSame = length groups == 1
  unless allTheSame $ do
    process <- ask
    throwError $ Runtime_CannotMatch process groups

reduceProcess :: Chan -> Process -> Process -> RuntimeM Process

reduceProcess chan (Output x y p q) (Input v w r) = do
  checkChannels [chan, x, v]
  checkChannels [y, w]
  use IOReduce $ Compose y p (Compose chan q r)
reduceProcess _ (Input _ _ _) (Output _ _ _ _) = swap

reduceProcess chan (OutputT x p) (InputT y q) = do
  checkChannels [chan, x, y]
  use TypeIOReduce $ Compose chan p q

reduceProcess _ (InputT _ _) (OutputT _ _) = swap
reduceProcess _ _ _ = do
  -- input <- ask
  -- liftIO $ print $ findMatches input

  stuck


rotateLeft :: Process -> Process
rotateLeft (Compose x p (Compose y q r)) =
  Compose y (Compose x p q) r
rotateLeft others = others


-- (Compose chan _ (Process (Output x y p q) f l) (Process (Input v w r) g m)) -> do
-- if chan == x && chan == v && y == w
--   then step IOReduce $ Process (Compose y Nothing p (Process (Compose chan Nothing q r) free NoLoc)) free loc
--   else throwError $ Runtime_CannotMatch chan x v
-- (Compose chan _ (Process (Input v w r) g m) (Process (Output x y p q) f l)) -> do
-- step Swap $ Process (Compose chan Nothing (Process (Input v w r) g m) (Process (Output x y p q) f l)) free loc
--
-- (Compose chan _ (Process (OutputT x t p) f l) (Process (InputT y u q) g m)) -> do
-- if chan == x && chan == y
--   then step TypeIOReduce $ Process (Compose chan Nothing p (substituteType u t q)) free loc
--   else throwError $ Runtime_CannotMatch chan x y
-- (Compose chan _ (Process (InputT y u q) g m) (Process (OutputT x t p) f l)) -> do
-- step Swap $ Process (Compose chan Nothing (Process (OutputT x t p) f l) (Process (InputT y u q) g m)) free loc
--
-- (Compose chan _ p q) -> do
-- reducedP <- reduce p
-- pHasReduced <- hasReduced
-- if pHasReduced
--   then wrap $ Compose chan Nothing reducedP q
--   else do
--     reducedQ <- reduce q
--     qHasReduced <- hasReduced
--     if qHasReduced
--       then wrap $ Compose chan Nothing reducedP reducedQ
--       else stuck


--
-- compose :: Chan -> Process -> Process -> Proc
-- compose chan (OutputT x t p) (InputT y u q) = Compose chan Nothing p (substituteType u t q)
--
--
-- creep :: Match -> Process -> Proc
-- creep (Match chan 0 0) (Compose n _ p q) = compose n (toProcess p) (toProcess q)

--------------------------------------------------------------------------------
-- | Substition

-- substitute :: Process -> Chan -> Chan -> M Process
-- substitute (Call name) a b = do
--   p <- lookupProcess name
--   substitute p a b
-- substitute (Link x y) a b =
--   Link
--     <$> substChan x a b
--     <*> substChan y a b
-- substitute (Compose x t p q) a b =
--   Compose
--     <$> return x
--     <*> return t
--     <*> (if x == a then return p else substitute p a b)
--     <*> (if x == a then return q else substitute q a b)
-- substitute (Output x y p q) a b =
--   Output
--     <$> substChan x a b
--     <*> substChan y a b
--     <*> substitute p a b
--     <*> substitute q a b
-- substitute (Input x y p) a b =
--   Input
--     <$> substChan x a b
--     <*> (if y == a then return y else substChan y a b)
--     <*> (if y == a then return p else substitute p a b)
-- substitute (SelectL x p) a b =
--   SelectL
--     <$> substChan x a b
--     <*> substitute p a b
-- substitute (SelectR x p) a b =
--   SelectR
--     <$> substChan x a b
--     <*> substitute p a b
-- substitute (Choice x p q) a b =
--   Choice
--     <$> substChan x a b
--     <*> substitute p a b
--     <*> substitute q a b
-- substitute (Accept x y p) a b =
--   Accept
--     <$> substChan x a b
--     <*> (if y == a then return y else substChan y a b)
--     <*> (if y == a then return p else substitute p a b)
-- substitute (Request x y p) a b =
--   Request
--     <$> substChan x a b
--     <*> substChan y a b
--     <*> substitute p a b
-- substitute (OutputT x t p) a b =
--   OutputT
--     <$> substChan x a b
--     <*> return t
--     <*> substitute p a b
-- substitute (InputT x v p) a b =
--   InputT
--     <$> substChan x a b
--     <*> return v
--     <*> substitute p a b
-- substitute (EmptyOutput x) a b =
--   EmptyOutput
--     <$> substChan x a b
-- substitute (EmptyInput x p) a b =
--   EmptyInput
--     <$> substChan x a b
--     <*> substitute p a b
-- substitute (EmptyChoice x) a b =
--   EmptyChoice
--     <$> substChan x a b
-- substitute End _ _ = return End
-- substitute (Mix p q) a b =
--   Mix
--     <$> substitute p a b
--     <*> substitute q a b
--
--
-- substChan :: Chan -> Chan -> Chan -> M Chan
-- substChan a x y = return $ if a == x then y else a

--------------------------------------------------------------------------------
-- | RuntimeM

-- data RuntimeState = RuntimeState
--   { rtRule :: Maybe Rule
--   , rtInput :: Maybe Process
--   }
type RuntimeM = ExceptT RuntimeError (StateT (Maybe Rule) (ReaderT Process Core))

hasReduced :: RuntimeM Bool
hasReduced = do
  rule <- get
  case rule of
    Nothing -> return False
    Just _ -> return True

putRule :: Rule -> RuntimeM ()
putRule rule = do
  p <- hasReduced
  unless p $ put $ Just rule
--
-- instance Pretty Tree where
--   pretty (Node chan p q) =
--     "\\" <+> pretty chan <> line
--     <> indent 2 (vsep [pretty p, pretty q])
--   pretty (Leaf p) = pretty p


--------------------------------------------------------------------------------
-- | Rules

data Rule = Swap | TypeIOReduce | IOReduce

instance Pretty Rule where
  pretty Swap = "Swap"
  pretty TypeIOReduce = "Type input/output reduction"
  pretty IOReduce = "Input/output reduction"
