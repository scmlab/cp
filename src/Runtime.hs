{-# LANGUAGE OverloadedStrings, DeriveAnyClass                  #-}

module Runtime
  ( run
  , printStatus
  , step
  , findMatchingChannels
  )
where


  -- import Syntax.Concrete
import           Syntax.Abstract
-- import qualified TypeChecking.Unification      as U
-- import Pretty

-- import TypeChecking.Base
import           Base
import           Runtime.Reduction

-- import           Data.Loc                       ( Loc(..) )
-- import           Data.Maybe                     ( fromJust )
import qualified Data.List                     as List
-- import           Data.Text                      ( Text )
-- import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
-- import           Data.Map                       ( Map )
-- import qualified Data.Map                      as Map
import           Control.Monad.Reader
import           Control.Monad.State     hiding ( state )
import           Control.Monad.Except

import           Pretty


-- reduce the term by one step, and return the result and the applied rule
step :: Process -> M (Process, [(Rule, Process)])
step input = do
  (result, history) <- runReaderT (runStateT (runExceptT (reduce input)) [])
                                  input
  case result of
    Left  e      -> throwError $ RuntimeError e
    Right output -> return (output, history)

run :: Process -> M Process
run input = do
  (output, history) <- step input
  if null history then return output else run output     -- keep reducing

printStatus :: Process -> Rule -> M ()
printStatus output rule = do
  liftIO $ putDoc $ line
  liftIO $ putDoc $ paint $ pretty ("=>" :: String) <+> pretty rule <> line
  liftIO $ putDoc $ line <> pretty output <> line
  liftIO
    $  putDoc
    $  paint
    $  line
    <> pretty (show $ Set.toList $ findMatchingChannels output)
    <> line
 where
  paint :: Doc AnsiStyle -> Doc AnsiStyle
  paint = annotate (colorDull Blue)

--------------------------------------------------------------------------------
-- RuntimeM functions

stuck :: RuntimeM Process
stuck = ask

-- specify which Rule to use 
use :: Rule -> Process -> RuntimeM Process
use rule input = do
  modify ((:) (rule, input))
  return input

reduce :: Process -> RuntimeM Process
reduce input = case Set.lookupMin (findMatchingChannels input) of
  Nothing -> do
    liftIO $ putStrLn "Cannot find any matches"
    stuck
  Just match -> do
    liftIO $ print match
    case match of
      MatchingPair chan 0 0    -> reduceAt chan (reduceProcess chan) input
      MatchingPair chan 0 _    -> reduceAt chan (rotateLeft chan) input
      MatchingPair chan _ _    -> reduceAt chan (rotateRight chan) input

      MatchingLinkLeft  chan 0 -> reduceAt chan (axCut chan) input
      MatchingLinkRight chan 0 -> reduceAt chan (swap chan) input
      MatchingLinkLeft  ____ _ -> error "[ MatchingLinkLeft ]"
      MatchingLinkRight ____ _ -> error "[ MatchingLinkRight ]"

swap :: Chan -> Process -> Process -> RuntimeM Process
swap chan p q = use Swap $ Compose chan q p

-- TODO, make `rotateLeft` and `rotateRight` the symmetrical
rotateLeft :: Chan -> Process -> Process -> RuntimeM Process
-- we want `p` and `q` both have the same channel as `x`
rotateLeft x p (Compose y q r) = if Set.member x (freeChans q)
  then use RotateLeft $ Compose y (Compose x p q) r
  else use Swap $ Compose x p (Compose y r q)
rotateLeft x p others = return $ Compose x p others

rotateRight :: Chan -> Process -> Process -> RuntimeM Process
rotateRight x (Compose y p q) r = use RotateRight $ Compose y p (Compose x q r)
rotateRight x others          p = return $ Compose x others p

-- see if the input Channels are all the same, throw error if that's not the case
checkChannels :: [Chan] -> RuntimeM ()
checkChannels channels = do
  let groups     = map head $ List.group channels
  let allTheSame = length groups == 1
  unless allTheSame $ do
    process <- ask
    throwError $ Runtime_CannotMatch process groups

axCut :: Chan -> Process -> Process -> RuntimeM Process
axCut chan (Link x y) q = if chan == x
  then use AxCutLeft $ subsitute x y q
  else use AxCutRight $ subsitute y x q
axCut _ p _ = error $ show p

-- What to do when there's a "matching Compose"
reduceProcess :: Chan -> Process -> Process -> RuntimeM Process

-- reduceProcess chan p@(Link w x) q@(Link _ y) = if chan == y
--   then use Swap $ Compose chan q p
--   else if chan == x then use AxCut $ subsitute y w q else error "[ panic ]"

-- reduceProcess chan (Link x y) q = do
--   checkChannels [chan, y]
--   use AxCut $ subsitute y x q

-- reduceProcess chan p q@(Link _ y) = do
--   checkChannels [chan, y]
--   use Swap $ Compose chan q p

reduceProcess chan (Output x y p q) (Input v w r) = do
  checkChannels [chan, x, v]
  checkChannels [y, w]
  use IOReduce $ Compose y p (Compose chan q r)

reduceProcess chan p@(Input _ _ _) q@(Output _ _ _ _) =
  use Swap $ Compose chan q p

reduceProcess chan (OutputT x p) (InputT y q) = do
  checkChannels [chan, x, y]
  use TypeIOReduce $ Compose chan p q

reduceProcess chan p@(InputT _ _) q@(OutputT _ _) = use Swap $ Compose chan q p

reduceProcess chan (Accept x y p) (Request x' y' q) = do
  checkChannels [chan, x, x']
  checkChannels [y, y']
  use AccReqReduce $ Compose y p q
  -- p' <- use AccReqReduce $ Compose y p q
  -- use AccContract $ Accept x y p'

reduceProcess _chan _ _ = stuck

-- Dive into a Process and act on the "matching Compose" (i.e. Match _ 0 0)
reduceAt
  :: Chan                                     -- the channel to target on Compose
  -> (Process -> Process -> RuntimeM Process) -- what to do when we hit the targetting Compose
  -> Process                                  -- input
  -> RuntimeM Process                         -- output
reduceAt target f (Compose x p q) = if target == x
  then f p q
  else Compose <$> pure x <*> reduceAt target f p <*> reduceAt target f q
reduceAt target f (Output x y p q) =
  Output <$> pure x <*> pure y <*> reduceAt target f p <*> reduceAt target f q
reduceAt target f (Input x y p) =
  Input <$> pure x <*> pure y <*> reduceAt target f p
reduceAt target f (SelectL x p) = SelectL <$> pure x <*> reduceAt target f p
reduceAt target f (SelectR x p) = SelectR <$> pure x <*> reduceAt target f p
reduceAt target f (Choice x p q) =
  Choice <$> pure x <*> reduceAt target f p <*> reduceAt target f q
reduceAt target f (Accept x y p) =
  Accept <$> pure x <*> pure y <*> reduceAt target f p
reduceAt target f (Request x y p) =
  Request <$> pure x <*> pure y <*> reduceAt target f p
reduceAt target f (OutputT x p  ) = OutputT <$> pure x <*> reduceAt target f p
reduceAt target f (InputT  x p  ) = InputT <$> pure x <*> reduceAt target f p
reduceAt _      _ (EmptyOutput x) = EmptyOutput <$> pure x
reduceAt target f (EmptyInput x p) =
  EmptyInput <$> pure x <*> reduceAt target f p
reduceAt _ _ (EmptyChoice x) = EmptyChoice <$> pure x
reduceAt target f (Mix p q) =
  Mix <$> reduceAt target f p <*> reduceAt target f q
reduceAt _ _ others = return others


--------------------------------------------------------------------------------
-- | Substition

-- substitute :: Process -> Chan -> Chan -> M Process
-- substitute (Atom n ns) a b = Link <$> substChan x a b <*> substChan y a b
-- substitute (Link x y) a b = Link <$> substChan x a b <*> substChan y a b
-- substitute (Compose x p q) a b =
--   Compose
--     <$> return x
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
-- substitute (SelectL x p) a b = SelectL <$> substChan x a b <*> substitute p a b
-- substitute (SelectR x p) a b = SelectR <$> substChan x a b <*> substitute p a b
-- substitute (Choice x p q) a b =
--   Choice <$> substChan x a b <*> substitute p a b <*> substitute q a b
-- substitute (Accept x y p) a b =
--   Accept
--     <$> substChan x a b
--     <*> (if y == a then return y else substChan y a b)
--     <*> (if y == a then return p else substitute p a b)
-- substitute (Request x y p) a b =
--   Request <$> substChan x a b <*> substChan y a b <*> substitute p a b
-- substitute (OutputT x p) a b = OutputT <$> substChan x a b <*> substitute p a b
-- substitute (InputT x p) a b = InputT <$> substChan x a b <*> substitute p a b
-- substitute (EmptyOutput x) a b = EmptyOutput <$> substChan x a b
-- substitute (EmptyInput x p) a b =
--   EmptyInput <$> substChan x a b <*> substitute p a b
-- substitute (EmptyChoice x) a b = EmptyChoice <$> substChan x a b
-- substitute End             _ _ = return End
-- substitute (Mix p q)       a b = Mix <$> substitute p a b <*> substitute q a b


-- substChan :: Chan -> Chan -> Chan -> M Chan
-- substChan a x y = return $ if a == x then y else a

--------------------------------------------------------------------------------
-- | RuntimeM

-- data RuntimeState = RuntimeState
--   { rtRule :: Maybe Rule
--   , rtInput :: Maybe Process
--   }
type RuntimeM
  = ExceptT RuntimeError (StateT [(Rule, Process)] (ReaderT Process M))


--------------------------------------------------------------------------------
-- | Rules

data Rule = Swap | RotateLeft | RotateRight
  | IOReduce | TypeIOReduce
  | AccReqReduce
  | AccContract
  | AxCutLeft -- NOTE: dubious cut rule
  | AxCutRight

instance Pretty Rule where
  pretty Swap         = "Swap"
  pretty RotateLeft   = "Rotate left"
  pretty RotateRight  = "Rotate right"
  pretty IOReduce     = "Input/output reduction"
  pretty TypeIOReduce = "Type input/output reduction"
  pretty AccReqReduce = "Accept/request reduction"
  pretty AccContract  = "Accept contraction"
  pretty AxCutLeft =
    "Link reduction (AxCut2, this cut rule is not present in the paper)"
  pretty AxCutRight = "Link reduction (AxCut)"
