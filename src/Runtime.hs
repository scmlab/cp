module Runtime where


-- import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
-- import qualified Syntax.Concrete  hiding (Session(..), Type(..), TypeVar(..))
-- import qualified Syntax.Abstract as A
import Syntax.Concrete (ToAbstract(..))
-- import Syntax.Abstract (Session, Type(..), TypeVar(..))
import Syntax.Abstract hiding (Session)

import TypeChecking.Base
import Base

-- import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except


lookupProcess :: Name -> M Process
lookupProcess name = do
  definition <- gets replDefinitions
  case Map.lookup name (Map.mapKeys toAbstract definition) of
    Nothing -> throwError $ RuntimeError $ Runtime_NotInScope name
    Just (Annotated _ p _) -> return (toAbstract p)
    Just (Unannotated _ p) -> return (toAbstract p)

reduce :: Process -> M Process
reduce (Call name) = lookupProcess name >>= reduce
reduce (Compose chan t p q) = do
  p' <- reduce p
  q' <- reduce q
  compose chan t p' q'
reduce others = return others

compose :: Chan -> (Maybe Type) -> Process -> Process -> M Process
-- expanding calls
compose chan t (Call name) q = do
  p <- lookupProcess name
  compose chan t p q >>= reduce
compose chan t p (Call name)
  = compose chan (fmap Dual t) (Call name) p >>= reduce
-- AxCut
compose chan t (Link x y) p
  | chan == x = substitute p x y >>= reduce
  | chan == y = substitute p y x >>= reduce
  | otherwise = reduce $ Compose chan t (Link x y) p
compose chan t p (Link x y)
  = compose chan (fmap Dual t) (Link x y) p >>= reduce
-- β⊗􏰀􏰀􏰀⅋
compose chan t (Output x y p q) (Input v w r)
  | chan == x && chan == v = compose y Nothing p (Compose x Nothing q r) >>= reduce
  | otherwise = reduce $ Compose chan t (Output x y p q) (Input v w r)
compose chan t (Input v w r) (Output x y p q)
  = compose chan (fmap Dual t) (Output x y p q) (Input v w r) >>= reduce
-- β⊕& left
compose chan t (SelectL x p) (Choice y q r)
  | chan == x && chan == y = compose x Nothing p q >>= reduce
  | otherwise = reduce $ Compose chan t (SelectL x p) (Choice y q r)
compose chan t (Choice y q r) (SelectL x p)
  = compose chan (fmap Dual t) (SelectL x p) (Choice y q r) >>= reduce
-- β⊕& right
compose chan t (SelectR x p) (Choice y q r)
  | chan == x && chan == y = compose x Nothing p r >>= reduce
  | otherwise = reduce $ Compose chan t (SelectR x p) (Choice y q r)
compose chan t (Choice y q r) (SelectR x p)
  = compose chan (fmap Dual t) (SelectR x p) (Choice y q r) >>= reduce
-- β!?
-- compose chan t (InputT x y p) (OutputT v w q)
--   | chan == x && chan == v = return $ Compose y Nothing p q
--   | otherwise = return $ Compose chan t (InputT x y p) (OutputT v w q)
-- compose chan t (OutputT v w q) (InputT x y p)
--   = compose chan (fmap Dual t) (InputT x y p) (OutputT v w q)
-- β1⊥
compose chan t (EmptyOutput x) (EmptyInput y p)
  | chan == x && chan == y = reduce p
  | otherwise = reduce $ Compose chan t (EmptyOutput x) (EmptyInput y p)
compose chan t (EmptyInput y p) (EmptyOutput x)
  = compose chan (fmap Dual t) (EmptyOutput x) (EmptyInput y p) >>= reduce
compose chan t p q = return $ Compose chan t p q

-- reduce :: Process -> M Process
-- reduce process = case process of
--   Call name -> lookupProcess name
--   -- AxCut
--   Compose chan _ (Link x y) p ->
--           if chan == x then substitute p x y
--     else  if chan == y then substitute p y x
--     else  return process
--   -- AxCut + Swap
--   Compose chan t p (Link x y) -> reduce (swap chan t p (Link x y))
--   --
-- reduce _ = undefined

-- Swap
swap :: Chan -> (Maybe Type) -> Process -> Process -> Process
swap x Nothing p q = Compose x Nothing q p
swap x (Just t) p q = Compose x (Just (Dual t)) q p

substitute :: Process -> Chan -> Chan -> M Process
substitute (Call name) a b = do
  p <- lookupProcess name
  substitute p a b
substitute (Link x y) a b =
  Link
    <$> substChan x a b
    <*> substChan y a b
substitute (Compose x t p q) a b =
  Compose
    <$> return x
    <*> return t
    <*> (if x == a then return p else substitute p a b)
    <*> (if x == a then return q else substitute q a b)
substitute (Output x y p q) a b =
  Output
    <$> substChan x a b
    <*> substChan y a b
    <*> substitute p a b
    <*> substitute q a b
substitute (Input x y p) a b =
  Input
    <$> substChan x a b
    <*> (if y == a then return y else substChan y a b)
    <*> (if y == a then return p else substitute p a b)
substitute (SelectL x p) a b =
  SelectL
    <$> substChan x a b
    <*> substitute p a b
substitute (SelectR x p) a b =
  SelectR
    <$> substChan x a b
    <*> substitute p a b
substitute (Choice x p q) a b =
  Choice
    <$> substChan x a b
    <*> substitute p a b
    <*> substitute q a b
substitute (Accept x y p) a b =
  Accept
    <$> substChan x a b
    <*> (if y == a then return y else substChan y a b)
    <*> (if y == a then return p else substitute p a b)
substitute (Request x y p) a b =
  Request
    <$> substChan x a b
    <*> substChan y a b
    <*> substitute p a b
substitute (OutputT x t p) a b =
  OutputT
    <$> substChan x a b
    <*> return t
    <*> substitute p a b
substitute (InputT x v p) a b =
  InputT
    <$> substChan x a b
    <*> return v
    <*> substitute p a b
substitute (EmptyOutput x) a b =
  EmptyOutput
    <$> substChan x a b
substitute (EmptyInput x p) a b =
  EmptyInput
    <$> substChan x a b
    <*> substitute p a b
substitute (EmptyChoice x) a b =
  EmptyChoice
    <$> substChan x a b
substitute End _ _ = return End
substitute (Mix p q) a b =
  Mix
    <$> substitute p a b
    <*> substitute q a b


substChan :: Chan -> Chan -> Chan -> M Chan
substChan a x y = return $ if a == x then y else a
