module TypeChecking.Inference2 where

import qualified Syntax.Concrete as C
import Syntax.Concrete (HasDual(..))
import Syntax.TypeChecking

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Data.List as List
import Data.Loc (Loc)
-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | State

type Var = C.TermName Loc
type Term = C.Process Loc

-- Γ, Δ, bunch of Var - Type pair
-- type Context = Map Var Type
type CtxVar = Int

data InferState = InferState
  {
    -- stContext   :: Context
  -- type variables
  --   stTypeSet   :: Set Type         -- bunch of types
  -- , stTypeCount :: Int              -- for type variables
  -- context variables
    -- stCtxMap    :: IntMap Context      -- bunch of contexts
    stTarget    :: Context
  , stCtxCount  :: CtxVar              -- for context variables
  } deriving (Show)

initialInferState :: InferState
initialInferState = InferState (Context Map.empty (Set.singleton 0)) 1  -- Set.empty 0

freshCtx :: InferM CtxVar
freshCtx = do
  i <- gets stCtxCount
  modify $ \ st -> st { stCtxCount = i + 1 }
  return $ i

putTarget :: Context -> InferM ()
putTarget x = modify $ \ st -> st { stTarget = x }

--------------------------------------------------------------------------------
-- | Error

data InferError
  = General Text
  | VarNotAssumed Var
  | VarNotInContext Var Context
  | OverlappedContext Context
  | ContextShouldBeTheSame Context Context
  | ContextShouldAllBeRequests Context
  | CannotUnify Type Type
  | CannotUnifyContext Context Context
--   | VarNotFresh Var (Maybe Term)
  deriving (Show)

--------------------------------------------------------------------------------
-- | Monad

type InferM = ExceptT InferError (State InferState)

--------------------------------------------------------------------------------
-- |

infer :: Term -> InferM (Map Var Type)
infer term = do
  ctx <- gets stTarget
  _ <- infer' term ctx
  undefined

infer' :: Term -> Context -> InferM Context
infer' (C.Output x y p q _) ctx = do
  infer'
infer' (C.EmptyOutput x _) ctx = do
  unify ctx (insert x One empty)
infer' _ _ = undefined

data Context = Context
                (Map Var Type) -- e : T, ....
                (Set CtxVar) -- Γ, Δ, ...
                deriving (Show)

empty :: Context
empty = Context Map.empty Set.empty

isEmpty :: Context -> Bool
isEmpty (Context m s) = Map.null m && Set.null s

insert :: Var -> Type -> Context -> Context
insert x t (Context m s) = Context (Map.insert x t m) s

add :: CtxVar -> Context -> Context
add var (Context m s) = Context m (Set.insert var s)

-- unifyType :: Type -> Type -> InferM Type


-- substitute the context of the target
substitute :: CtxVar -> Context -> InferM ()
substitute var ctx = do
  target <- gets stTarget
  putTarget (subst var ctx target)

-- substitute references to "var" in the latter context with the former context
subst :: CtxVar -> Context -> Context -> Context
subst var (Context m s) (Context n t) = if Set.member var t
  then Context (mergePairs m n) (Set.union s $ Set.delete var t)
  else Context n t

mergePairs :: Map Var Type -> Map Var Type -> Map Var Type
mergePairs = Map.merge
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.mapMaybeMissing (\_ x -> Just x))
  (Map.zipWithMaybeMatched (\_ x _ -> Just x))

  --   case viewContext ctx of
  -- Empty      -> Context m s
  -- NoPairs s' -> Context m (Set.update )

-- substitute var ctx = do
--   modify $ \ st -> st { stCtxMap = IntMap.update (\_ -> Just ctx) var (stCtxMap st) }

unify :: Context -> Context -> InferM Context
unify a b = case (viewContext a, viewContext b) of
  -- nothing to do
  (Empty         , Empty         ) -> return empty
  -- throw if one is empty but the another has a pair
  (Empty         , NoPairs _     ) -> throwError $ CannotUnifyContext a b
  (Empty         , Everything _ _) -> throwError $ CannotUnifyContext a b
  (NoPairs s    , Empty         ) -> do
    forM_ s (flip substitute empty)
    return empty
  -- decree that s & t should both be of size 1
  (NoPairs s     , NoPairs t     ) -> undefined
    -- unless (Set.size s == 1 && Set.size t == 1) $
    --    throwError $ CannotUnifyContext a b
  (NoPairs _     , Everything _ _) -> return empty
  (Everything _ _, Empty         ) -> return empty
  (Everything _ _, NoPairs _     ) -> return empty
  (Everything _ _, Everything _ _) -> return empty


data ContextView  = Empty
                  | NoPairs (Set CtxVar)
                  | Everything (Map Var Type) (Set CtxVar)
                  deriving (Show)

viewContext :: Context -> ContextView
viewContext (Context m s)
  |      Map.null m  &&      Set.null s  = Empty
  |      Map.null m  && not (Set.null s) = NoPairs s
  -- | not (Map.null m) &&     (Set.null s) = PairsOnly m
  | otherwise                            = Everything m s


-- unify :: Context -> Context -> InferM Context
-- unify (Has x t a) (Has y u b)   = if x == y
--                                     then do
--                                       v <- unifyType t u
--                                       c <- unifyType a b
--                                       return $ Has x v c
--                                     else do
--
-- unify (Has x t a) (Some b)      = Empty
-- unify (Has x t a) Empty         = throwError CannotUnify2
-- unify (Some a)    (Has x t b)   = Empty
-- unify (Some a)    (Some b)      = Some (unify a b)
-- unify (Some a)    Empty         = Empty
-- unify Empty       (Has x t a)   = throwError CannotUnify2
-- unify Empty       (Some a)      = Empty
-- unify Empty       Empty         = Empty



-- data Judgement = Judgement Term Ctx
--   deriving (Show)

-- -- ruleAx :: Judgement
--
-- -- unify ::
--
--
-- infer' :: Judgement -> InferM Judgement
-- -- infer' (Judgement (C.Link w x _) ctx) = ctx
-- infer' (Judgement (C.EmptyOutput x _) ctx) = do
--   unify ctx (Has x One Empty)
--   return Done
-- infer' _ = undefined
