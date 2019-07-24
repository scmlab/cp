{-# LANGUAGE OverloadedStrings, DeriveAnyClass                  #-}

module Runtime (evaluate) where


import Syntax.Binding
import qualified TypeChecking.Unification as U
-- import Pretty

-- import TypeChecking.Base
import Base
import Runtime.Reduction

import Data.Loc (Loc(..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad.Except

import Pretty

evaluate :: Process -> M Process
evaluate process = run process

run :: Process -> M Process
run input = do

  (result, usedRule) <- lift $ runReaderT (runStateT (runExceptT reduce) Nothing) input

  case result of
    Left e -> throwError $ RuntimeError e
    Right output -> do
      case usedRule of
        Nothing -> return output  -- stuck
        Just rule -> do

          printStatus input rule
          run output     -- keep reducing

printStatus :: Process -> Rule -> M ()
printStatus process rule = do
  liftIO $ putDoc $ line
  liftIO $ putDoc $ paint $ pretty ("=>" :: String) <+> pretty rule <> line
  liftIO $ putDoc $ line
    <> pretty (toTree process)
    <> line
  where
    paint :: Doc AnsiStyle -> Doc AnsiStyle
    paint = annotate (colorDull Blue)

stuck :: RuntimeM Proc
stuck = toProc <$> ask

swap :: RuntimeM Proc
swap = do
  putRule Swap
  Process input free loc <- ask
  case input of
    Compose chan t p q -> return $ Compose chan (fmap dual t) q p
    others -> return others

step :: Rule -> Proc -> RuntimeM Proc
step rule input = do
  p <- hasReduced
  if p
    then stuck
    else do
      putRule rule
      return input

reduce :: RuntimeM Process
reduce = do
  input <- ask
  case toProc input of
    Compose chan _ p q -> do
      output <- reduceProcess input chan (toProc p) (toProc q)
      return $ Process output undefined NoLoc
    others -> return input

-- checkChannels :: [Chan] -> RuntimeM ()
-- checkChannels


reduceProcess :: Process -> Chan -> Proc -> Proc -> RuntimeM Proc
reduceProcess input chan (OutputT x t p) (InputT y u q) = do
  if chan == x && chan == y
    then step TypeIOReduce $ Compose chan Nothing p (substituteType u t q)
    else throwError $ Runtime_CannotMatch chan x y
reduceProcess input chan (InputT x t p) (OutputT y u q) = swap
reduceProcess input chan p q = stuck



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
-- compose :: Chan -> Proc -> Proc -> Proc
-- compose chan (OutputT x t p) (InputT y u q) = Compose chan Nothing p (substituteType u t q)
--
--
-- creep :: Match -> Proc -> Proc
-- creep (Match chan 0 0) (Compose n _ p q) = compose n (toProc p) (toProc q)

--------------------------------------------------------------------------------
-- | Substition

substituteType :: TypeVar -> Type -> Process -> Process
substituteType old new (Process proc free loc) =
  Process result free loc
  where
    result = case proc of
              Compose x t p q -> Compose x (fmap substT t) (subst p) (subst q)
              Output x y p q -> Output x y (subst p) (subst q)
              Input x y p -> Input x y (subst p)
              SelectL x p -> SelectL x (subst p)
              SelectR x p -> SelectR x (subst p)
              Choice x p q -> Choice x (subst p) (subst q)
              Accept x y p -> Accept x y (subst p)
              Request x y p -> Request x y (subst p)
              OutputT x t p -> OutputT x (substT t) (subst p)
              InputT x t p -> InputT x t (subst p)
              EmptyInput x p -> EmptyInput x (subst p)
              Mix p q -> Mix (subst p) (subst q)
              others -> others
    subst = substituteType old new
    substT = U.substitute old new

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

takeRule :: RuntimeM (Maybe Rule)
takeRule = do
  result <- get
  case result of
    Nothing -> return Nothing
    Just rule -> do
      put Nothing
      return $ Just rule

--------------------------------------------------------------------------------
-- | Tree representation of Process


data Tree
    = Node Chan Tree Tree
    | Leaf Process
  deriving (Show)

toTree :: Process -> Tree
toTree (Process (Compose chan _ p q) _ _) =
  Node chan (toTree p) (toTree q)
toTree others =
  Leaf others

-- fromTree :: Tree -> Process
-- fromTree (Node chan p q) = Process (Compose chan Nothing (fromTree p) (fromTree q)) undefined NoLoc
-- fromTree (Leaf p) = p

instance Pretty Tree where
  pretty (Node chan p q) =
    "\\" <+> pretty chan <> line
    <> indent 2 (vsep [pretty p, pretty q])
  pretty (Leaf p) = pretty p


--------------------------------------------------------------------------------
-- | Rules

data Rule = Swap | TypeIOReduce | IOReduce

instance Pretty Rule where
  pretty Swap = "Swap"
  pretty TypeIOReduce = "Type input/output reduction"
  pretty IOReduce = "Input/output reduction"
