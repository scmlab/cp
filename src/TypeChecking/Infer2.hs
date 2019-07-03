module TypeChecking.Infer2 where

import Syntax.Binding
import TypeChecking.Base
import qualified TypeChecking.Unification as U
import TypeChecking.Unification


-- import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Data.Loc (Loc(..), locOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace


infer :: Process -> InferM Session
infer process = case process of
  Call (Callee name _) _ -> do
    definitions <- lift $ lift $ gets stDefinitions
    case Map.lookup name definitions of
      Nothing -> error "[panic] Definition not found, this shouldn't happen at the type checking state"
      Just (Annotated _ _ t) -> return t
      Just (Unannotated _ p) -> infer p

  Link x y _ -> do

    return $ Map.fromList
      [ (x, Bot NoLoc)
      ]

  EmptyInput x p _ -> do

    x `notFreeIn` p

    session <- infer p

    -- unify (Bot NoLoc) t
    -- traceShow session (return ())

    return $ Map.fromList
      [ (x, Bot NoLoc)
      ]

  End _ -> return Map.empty


    -- sessionP <- infer p [] [x]
    --
    -- t <- extract x
    -- unify (Bot NoLoc) t

  -- Link x y _ -> do

  others -> error $ show others

  where
    notFreeIn :: Chan -> Process -> InferM ()
    notFreeIn channel term = do
      -- error $ show (freeVariables process, channel)
      let Chan _ name _ = channel
      when (name `Set.member` freeVariables term) $
        throwError $ ChanFound term channel

    -- unify the two given types, and update the give session.
    -- for better error message, make the former type be the expecting type
    -- and the latter be the given type
    unify :: Type -> Type -> InferM ()
    unify expected given = do
      let (result, subst) = U.unify expected given
      case result of
        Left (t, u) -> throwError $ TypeMismatch process expected given t u
        Right _ -> tell subst

    -- hasNo channels p = do
    --   process <- p
    --   let names = map (\(Chan n _) -> n) channels
    --   let present = Set.fromList names `Set.intersection` B.freeVariables process
    --   unless (Set.null present) $
    --     throwError $ ChanFound process present
    --   return process

--------------------------------------------------------------------------------
-- | InferM

type TypeVars = Map Chan TypeVar
type InferM = WriterT [Substitution] (StateT TypeVars TypeM)

runInferM :: TypeVars -> InferM a -> TypeM ((a, [Substitution]), TypeVars)
runInferM freeVars f = runStateT (runWriterT f) freeVars

inferProcess :: Process -> TypeM Session
inferProcess term = do
  ((result, substitutions), freeChannels) <- runInferM Map.empty (infer term)
  return $ Map.union (substitute substitutions result) (substitute substitutions (fmap (\c -> Var c (locOf c)) freeChannels))
  where
    substitute :: [Substitution] -> Session -> Session
    substitute = flip $ foldr $ \ (Substitute var t) -> Map.map (U.substitute var t)
