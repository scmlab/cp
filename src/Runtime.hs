module Runtime where


import Syntax.Binding
-- import Pretty

-- import TypeChecking.Base
import Base

import Data.Loc (Loc(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
-- import Debug.Trace

data RuntimeState = RuntimeState
  { rsBlocked :: Map Chan Process
  , rsPool :: Set Process
  }
type RuntimeM = ExceptT RuntimeError (StateT RuntimeState Core)

addBlocked :: Chan -> Process -> RuntimeM ()
addBlocked chan process = modify $ \st -> st { rsBlocked = Map.insert chan process (rsBlocked st) }

removeBlocked :: Chan -> RuntimeM ()
removeBlocked chan = modify $ \st -> st { rsBlocked = Map.delete chan (rsBlocked st) }

putBack :: Process -> RuntimeM ()
putBack (End _) = return ()
putBack others  = modify $ \st -> st { rsPool = Set.insert others (rsPool st) }

draw :: RuntimeM (Maybe Process)
draw = do
  result <- Set.lookupMin <$> gets rsPool
  modify $ \st -> st { rsPool = Set.deleteMin (rsPool st) }
  return result

evaluate :: Process -> M Process
evaluate process = do
  result <- lift $ evalStateT
              (runExceptT run)
              $ RuntimeState
                  Map.empty
                  (Set.singleton process)
  case result of
    Left err -> throwError $ RuntimeError err
    Right p -> return p

run :: RuntimeM Process
run = do
  next <- draw
  case next of
    Nothing -> do
      -- blockedSize <- Map.size <$> gets rsBlocked
      -- poolSize <- Set.size <$> gets rsPool
      -- liftIO $ putStrLn $ show (blockedSize, poolSize) ++ " ================="
      -- liftIO $ print $ pretty next
      --
      -- blocked <- Map.toList <$> gets rsBlocked
      -- forM_ blocked $ \(c, p) -> do
      --   liftIO $ print $ pretty (c, p)
      -- liftIO $ putStrLn "===================="

      -- gets rsBlocked >>= error . show
      return $ End NoLoc
    Just process -> do
      digest process
      run

  -- x[u] . u[] . end
  -- x[u] . x[v] . v[] . end
  -- x[u] . x[v] . x(w) . x() . w() . end
  -- x(u) . x(v) . x[w] . u() . v() . w[] . end
  -- x(u) . x(v) . x[w] . u() . x[] . end


digest :: Process -> RuntimeM ()
digest process = case process of
  (Call callee _) -> do
    lookupCallee callee >>= putBack
  (Link _ _ _) -> undefined
  (Compose _ _ p q _) -> do
    putBack p
    putBack q
  (Output x y p q _) -> do
    blocked <- Map.lookup x <$> gets rsBlocked
    case blocked of
      Nothing -> addBlocked x process
      Just (Input _ y' r _) -> do
        if y == y'
          then do
            removeBlocked x
            putBack p
            putBack q
            putBack r
          else throwError $ Runtime_CannotMatch x y y'
      _ -> throwError $ Runtime_Stuck process
  (Input x y p _) -> do
    blocked <- Map.lookup x <$> gets rsBlocked
    case blocked of
      Nothing -> addBlocked x process
      Just (Output _ y' q r _) -> do
        if y == y'
          then do
            removeBlocked x
            putBack p
            putBack q
            putBack r
          else throwError $ Runtime_CannotMatch x y y'
      _ -> throwError $ Runtime_Stuck process
  (EmptyOutput x _) -> do
    blocked <- Map.lookup x <$> gets rsBlocked
    case blocked of
      Nothing -> addBlocked x process
      Just (EmptyInput _ p _) -> do
        removeBlocked x
        putBack p
      _ -> throwError $ Runtime_Stuck process
  (EmptyInput x p _) -> do
    blocked <- Map.lookup x <$> gets rsBlocked
    case blocked of
      Nothing -> addBlocked x process
      Just (EmptyOutput _ _) -> do
        removeBlocked x
        putBack p
      _ -> throwError $ Runtime_Stuck process
  others -> error $ show others

-- -- run (Compose chan _ p q _) =
-- run others = return others
  -- next <- Set.lookupMin <$> gets rsPool
  -- traceShow (next) (return ())
  -- case next of
  --   Nothing -> throwError $ Runtime_PoolIsEmpty
  --   Just process -> do
  --     poolSize <- Set.size <$> gets rsPool
  --     case poolSize of
  --       1 -> return process
  --       _ -> digest process


-- digest :: Process -> RuntimeM Process
-- digest = undefined
-- digest (Call name) = do
--   lookupProcess name >>= insertPool
--   run
-- -- digest (Link x y) = do
-- digest End = undefined



lookupCallee :: Callee -> RuntimeM Process
lookupCallee (Callee name _) = do
  definitions <- lift $ lift $ gets replDefinitions
  case Map.lookup name definitions of
    Nothing -> throwError $ Runtime_NotInScope name
    Just (Annotated _ p _) -> return p
    Just (Unannotated _ p) -> return p



-- reduce :: Process -> M Process
-- reduce (Call name) = lookupProcess name >>= reduce
-- reduce (Compose chan t p q) = do
--   p' <- reduce p
--   q' <- reduce q
--   -- compose chan t p' q'
-- reduce others = return others
-- reduce = undefined
--
-- reduce :: Process -> M Process
-- reduce (Call name) = lookupProcess name >>= reduce
-- reduce (Compose chan t p q) = do
--   p' <- reduce p
--   q' <- reduce q
--   compose chan t p' q'
-- reduce others = return others
--
-- compose :: Chan -> (Maybe Type) -> Process -> Process -> M Process
-- -- expanding calls
-- compose chan t (Call name) q = do
--   p <- lookupProcess name
--   compose chan t p q >>= reduce
-- compose chan t p (Call name)
--   = compose chan (fmap Dual t) (Call name) p >>= reduce
-- -- AxCut
-- compose chan t (Link x y) p
--   | chan == x = substitute p x y >>= reduce
--   | chan == y = substitute p y x >>= reduce
--   | otherwise = reduce $ Compose chan t (Link x y) p
-- compose chan t p (Link x y)
--   = compose chan (fmap Dual t) (Link x y) p >>= reduce
-- -- β⊗􏰀􏰀􏰀⅋
-- compose chan t (Output x y p q) (Input v w r)
--   | chan == x && chan == v = compose y Nothing p (Compose x Nothing q r) >>= reduce
--   | otherwise = reduce $ Compose chan t (Output x y p q) (Input v w r)
-- compose chan t (Input v w r) (Output x y p q)
--   = compose chan (fmap Dual t) (Output x y p q) (Input v w r) >>= reduce
-- -- β⊕& left
-- compose chan t (SelectL x p) (Choice y q r)
--   | chan == x && chan == y = compose x Nothing p q >>= reduce
--   | otherwise = reduce $ Compose chan t (SelectL x p) (Choice y q r)
-- compose chan t (Choice y q r) (SelectL x p)
--   = compose chan (fmap Dual t) (SelectL x p) (Choice y q r) >>= reduce
-- -- β⊕& right
-- compose chan t (SelectR x p) (Choice y q r)
--   | chan == x && chan == y = compose x Nothing p r >>= reduce
--   | otherwise = reduce $ Compose chan t (SelectR x p) (Choice y q r)
-- compose chan t (Choice y q r) (SelectR x p)
--   = compose chan (fmap Dual t) (SelectR x p) (Choice y q r) >>= reduce
-- -- β!?
-- -- compose chan t (InputT x y p) (OutputT v w q)
-- --   | chan == x && chan == v = return $ Compose y Nothing p q
-- --   | otherwise = return $ Compose chan t (InputT x y p) (OutputT v w q)
-- -- compose chan t (OutputT v w q) (InputT x y p)
-- --   = compose chan (fmap Dual t) (InputT x y p) (OutputT v w q)
-- -- β1⊥
-- compose chan t (EmptyOutput x) (EmptyInput y p)
--   | chan == x && chan == y = reduce p
--   | otherwise = reduce $ Compose chan t (EmptyOutput x) (EmptyInput y p)
-- compose chan t (EmptyInput y p) (EmptyOutput x)
--   = compose chan (fmap Dual t) (EmptyOutput x) (EmptyInput y p) >>= reduce
-- compose chan t p q = return $ Compose chan t p q
--
-- -- reduce :: Process -> M Process
-- -- reduce process = case process of
-- --   Call name -> lookupProcess name
-- --   -- AxCut
-- --   Compose chan _ (Link x y) p ->
-- --           if chan == x then substitute p x y
-- --     else  if chan == y then substitute p y x
-- --     else  return process
-- --   -- AxCut + Swap
-- --   Compose chan t p (Link x y) -> reduce (swap chan t p (Link x y))
-- --   --
-- -- reduce _ = undefined
--
-- -- Swap
-- swap :: Chan -> (Maybe Type) -> Process -> Process -> Process
-- swap x Nothing p q = Compose x Nothing q p
-- swap x (Just t) p q = Compose x (Just (Dual t)) q p
--
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
