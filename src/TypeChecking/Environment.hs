module TypeChecking.Environment where

import qualified Syntax.Concrete as C
import Syntax.Concrete (HasDual(..))
import Syntax.TypeChecking

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Data.Loc (Loc)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Monad & Types

type Var = C.TermName Loc
type Term = C.Process Loc

-- Γ, Δ, bunch of Var - Type pair
type Context = Map Var Type

data EnvState = EnvState
  { stContexts  :: Map Int Context  -- bunch of contexts
  , stCtxCount  :: Int              -- for context variables
  , stTypes     :: Set Type         -- bunch of types
  , stTypeCount :: Int              -- for type variables
  } deriving (Show)

initialEnvState :: EnvState
initialEnvState = EnvState Map.empty 0 Set.empty 0
--
data InferError
  = General String
  | VarNotFoundInTypes Var
  | VarNotFoundInContext Var Context
  | OverlappedContext Context
  | CannotUnify Type Type
--   | VarNotFresh Var (Maybe Term)
  deriving (Show)
--
type EnvM = ExceptT InferError (State EnvState)

putTypes :: Set Type -> EnvM ()
putTypes x = modify $ \ st -> st { stTypes = x }

insertToTypes :: Type -> EnvM ()
insertToTypes t = modify $ \ st -> st { stTypes = Set.insert t (stTypes st) }

putContexts :: Map Int Context -> EnvM ()
putContexts x = modify $ \ st -> st { stContexts = x }

getFreshType :: EnvM Type
getFreshType = do
  i <- gets stTypeCount
  modify $ \ st -> st { stTypeCount = i + 1 }
  return $ Var (Pos i)

-- addToContext :: Var -> Prop -> EnvM ()
-- addToContext var prop = modify $ \ st -> st { stContext = Map.insert var prop (stContext st) }

--------------------------------------------------------------------------------
-- |

-- contextShouldBeEmpty :: EnvM ()
-- contextShouldBeEmpty = do
--   ctx <- gets stContext
--   unless (Set.null ctx) $ throwError ContextShouldBeEmpty

infer :: Term -> EnvM Context
infer (C.Link x y _) = do
  t <- assumeType
  -- u <- lookupType y
  -- (t, dual t) <- unifyOpposite t u
  return $ Map.fromList
    [ (x, t)
    , (y, dual t)
    ]
infer (C.Compose x p q _) = do
  ctxP <- Map.delete x <$> infer p
  ctxQ <- Map.delete x <$> infer q
  checkOverlappedContext ctxP ctxQ
  return $ mergeContext ctxP ctxQ
infer (C.Output x y p q _) = do
  ctxP <- infer p
  ctxQ <- infer q
  typeX <- lookup x ctxQ
  typeY <- lookup y ctxP
  return $ Map.insert x (Times typeY typeX) $
    mergeContext (Map.delete y ctxP) (Map.delete x ctxQ)

infer (C.Input x y p _) = do
  ctx <- infer p
  typeX <- lookup x ctx
  typeY <- lookup y ctx
  return $ Map.insert x (Par typeY typeX)
    $ Map.delete y
    $ Map.delete x ctx

-- infer (C.SelectL x p _) = do
--   ctx <- infer p

infer (C.SelectR x p _) = do
  ctx <- infer p
  a <- lookup x ctx
  b <- assumeType
  return  $ Map.insert x (Plus a b)
          $ Map.delete x ctx

infer (C.Choice x p q _) = undefined
infer (C.Accept x y p _) = undefined
infer (C.Request x y p _) = undefined
infer (C.EmptyOutput x _) = do
  return $ Map.fromList
    [ (x, One)
    ]
infer (C.EmptyInput x p _) = do
  a <- infer p
  return $ Map.insert x Bot a
infer (C.EmptyChoice x _) = undefined
  -- return $ Map.insert x Top
--   a <- infer p
--   b <- infer q


assumeType :: EnvM Type
assumeType = do
  t <- getFreshType
  insertToTypes t
  return t


-- unsafe, does not check if contexts are overlapping
mergeContext :: Context -> Context -> Context
mergeContext = Map.merge
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.zipWithMaybeMatched (\_ x _ -> Just x))

lookup :: Var -> Context -> EnvM Type
lookup var ctx = do
  case Map.lookup var ctx of
    Nothing -> throwError $ VarNotFoundInContext var ctx
    Just t -> return t

-- lookupType :: Var -> EnvM Type
-- lookupType var = do
--   types <- gets stTypes
--   case Map.lookup var types of
--     Nothing -> throwError $ VarNotFoundInTypes var
--     Just t -> return t

-- substitute type variables with concrete types or other type variables
substituteType :: Index -> Type -> EnvM ()
substituteType i t = do
  types <- gets stTypes
  putTypes    $ Set.fromList $ map substType $ Set.toList types
  ctxs <- gets stContexts
  putContexts $ fmap substCtx  ctxs

  where
    substType :: Type -> Type
    substType (Var j) = if i == j then t else Var j
    substType others = others

    substCtx :: Context -> Context
    substCtx ctx = fmap substType ctx

unifyOpposite :: Type -> Type -> EnvM (Type, Type)
unifyOpposite (Var i) (Var j) = do
  let i' = Var (dual i)
  -- replace j with the opposite of i
  substituteType j i'
  return (Var i, i')
unifyOpposite (Var i) u = do
  substituteType i (dual u)
  return (dual u, u)
unifyOpposite t (Var i) = do
  substituteType i (dual t)
  return (t, dual t)
unifyOpposite t u = if t == dual u
  then throwError $ CannotUnify t u
  else return (t, u)

checkOverlappedContext :: Context -> Context -> EnvM ()
checkOverlappedContext a b = do
  let overlapped = Map.difference a b
  unless (Map.null overlapped) $
    throwError $ OverlappedContext overlapped
