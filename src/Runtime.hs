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
  (result, usedRule) <- runReaderT (runStateT (runExceptT (reduce input)) Nothing) input

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

-- swap :: RuntimeM Process
-- swap = do
--   putRule Swap
--   input <- ask
--   case input of
--     Compose chan p q -> return $ Compose chan q p
--     others -> return others

use :: Rule -> Process -> RuntimeM Process
use rule input = do
  p <- hasReduced
  if p
    then stuck
    else do
      putRule rule
      return input

reduce :: Process -> RuntimeM Process
reduce input = do
  case Set.lookupMax (findMatches input) of
    Nothing -> do
      liftIO $ print $ findMatches input
      stuck
    Just match@(Match chan _ _) -> do
      liftIO $ print match
      reduceAt chan (f match) input
      where
        f :: Match  -> Process -> Process -> RuntimeM Process
        f (Match chan 0 0) p q = reduceProcess chan p q
        f (Match chan 0 _) p q = rotateLeft chan p q
        f (Match chan _ 0) p q = rotateRight chan p q

rotateLeft :: Chan -> Process -> Process -> RuntimeM Process
-- we want `p` and `q` both have the same channel as `x`
rotateLeft x p (Compose y q r) =
  if Set.member x (freeChans q)
    then use RotateLeft $ Compose y (Compose x p q) r
    else use Swap $ Compose x p (Compose y r q)
rotateLeft x p others = return $ Compose x p others

rotateRight :: Chan -> Process -> Process -> RuntimeM Process
rotateRight x (Compose y p q) r = do
  use RotateRight $ Compose y p (Compose x q r)
rotateRight x others p = return $ Compose x others p

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

reduceProcess chan p@(Input _ _ _) q@(Output _ _ _ _) =
  use Swap $ Compose chan q p

reduceProcess chan (OutputT x p) (InputT y q) = do
  checkChannels [chan, x, y]
  use TypeIOReduce $ Compose chan p q

reduceProcess chan p@(InputT _ _) q@(OutputT _ _) =
  use Swap $ Compose chan q p


reduceProcess chan (Accept x y p) (Request x' y' q) = do
  checkChannels [chan, x, x']
  checkChannels [y, y']
  use AccReqReduce $ Compose y p q

reduceProcess chan _ _ = do
  stuck

reduceAt
  :: Chan                                     -- the channel to target on Compose
  -> (Process -> Process -> RuntimeM Process) -- on Compose
  -> Process
  -> RuntimeM Process
reduceAt target f (Compose x p q) = do
  if target == x
    then f p q
    else Compose
          <$> pure x
          <*> reduceAt target f p
          <*> reduceAt target f q
reduceAt target f (Output x y p q) =
  Output
    <$> pure x
    <*> pure y
    <*> reduceAt target f p
    <*> reduceAt target f q
reduceAt target f (Input x y p) =
  Input
    <$> pure x
    <*> pure y
    <*> reduceAt target f p
reduceAt target f (SelectL x p) =
  SelectL
    <$> pure x
    <*> reduceAt target f p
reduceAt target f (SelectR x p) =
  SelectR
    <$> pure x
    <*> reduceAt target f p
reduceAt target f (Choice x p q) =
  Choice
    <$> pure x
    <*> reduceAt target f p
    <*> reduceAt target f q
reduceAt target f (Accept x y p) =
  Accept
    <$> pure x
    <*> pure y
    <*> reduceAt target f p
reduceAt target f (Request x y p) =
  Request
    <$> pure x
    <*> pure y
    <*> reduceAt target f p
reduceAt target f (OutputT x p) =
  OutputT
    <$> pure x
    <*> reduceAt target f p
reduceAt target f (InputT x p) =
  InputT
    <$> pure x
    <*> reduceAt target f p
reduceAt _ _ (EmptyOutput x) =
  EmptyOutput
    <$> pure x
reduceAt target f (EmptyInput x p) =
  EmptyInput
    <$> pure x
    <*> reduceAt target f p
reduceAt _ _ (EmptyChoice x) =
  EmptyChoice
    <$> pure x
reduceAt target f (Mix p q) =
  Mix
    <$> reduceAt target f p
    <*> reduceAt target f q
reduceAt _ _ others = return others


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
type RuntimeM = ExceptT RuntimeError (StateT (Maybe Rule) (ReaderT Process M))

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

--------------------------------------------------------------------------------
-- | Rules

data Rule = Swap | RotateLeft | RotateRight
  | IOReduce | TypeIOReduce
  | AccReqReduce

instance Pretty Rule where
  pretty Swap = "Swap"
  pretty RotateLeft = "Rotate left"
  pretty RotateRight = "Rotate right"
  pretty IOReduce = "Input/output reduction"
  pretty TypeIOReduce = "Type input/output reduction"
  pretty AccReqReduce = "Accept/request reduction"
