module Runtime where


-- import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
-- import qualified Syntax.Concrete  hiding (Session(..), Type(..), TypeVar(..))
import Syntax.Concrete (ToAbstract(..))
-- import Syntax.Abstract (Session, Type(..), TypeVar(..))
import Syntax.Abstract
import TypeChecking.Base (Definition(..))
import Base

-- import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except


reduce :: Process -> Process
-- reduce (Compose chan _ (Link x y) p) = undefined
reduce _ = undefined


substitute :: Process -> Chan -> Chan -> M Process
substitute (Call name) a b = do
  definition <- gets replDefinitions
  case Map.lookup name definition of
    Nothing -> throwError $ RuntimeError $ Runtime_DefnNotFound name
    Just (Annotated _ p _) -> substitute (toAbstract p) a b
    Just (Unannotated _ p) -> substitute (toAbstract p) a b
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
