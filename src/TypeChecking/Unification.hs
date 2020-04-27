module TypeChecking.Unification where

import           Syntax.Concrete                ( Type(..)
                                                , TypeVar(..)
                                                )
-- import Syntax.Abstract (Type(..), TypeVar(..))
import           Syntax.Base

import           Control.Monad.State
import           Control.Monad.Except

--------------------------------------------------------------------------------
-- | Unification

data Substitution = Substitute
                      TypeVar   -- substitutee
                      Type      -- substituter

instance Show Substitution where
  show (Substitute var t) = show var ++ " => " ++ show t

type UniError = (Type, Type)
type UniM = ExceptT UniError (State [Substitution])

unify :: Type -> Type -> Either (Type, Type) (Type -> Type)
unify a b =
  let (result, subst) = runState (runExceptT (run a b)) []
  in  case result of
        Left  err -> Left err
        Right _   -> Right $ normalize . (compose $ map apply subst)
          where
            -- compose a list of functions
                compose = foldl (flip (.)) id
 where
  run :: Type -> Type -> UniM Type
  run (Var i _) v = do
    modify ((:) (Substitute i v))
    return v
  run t (Var j _) = do
    modify ((:) (Substitute j t))
    return t
  -- run (Subst  t x u)  (Subst  v y w)  = Subst  <$> run t v <*> (modify ((:) (Substitute x (Var y))) >> return y) <*> run u w
  run (Dual t _) v          = dual <$> run t (dual v)
  run t          (Dual v _) = dual <$> run (dual t) v
  run (Times t u l) (Times v w l') =
    Times <$> run t v <*> run u w <*> pure (max l l')
  run (Par t u l) (Par v w l') =
    Par <$> run t v <*> run u w <*> pure (max l l')
  run (Plus t u l) (Plus v w l') =
    Plus <$> run t v <*> run u w <*> pure (max l l')
  run (With t u l) (With v w l') =
    With <$> run t v <*> run u w <*> pure (max l l')
  run (Acc t _             ) (Acc v _             ) = run t v
  run (Req t _             ) (Req v _             ) = run t v
  run (Exists _ u Nothing _) (Exists _ w Nothing _) = run u w
  -- happens when we are composing ∃ with ∀
  run (Exists _ (Var ghost _) (Just (witness, substituted)) _) (Exists var body Nothing _)
    = do
    -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run substituted (substitute var witness body)
  run (Exists var body Nothing _) (Exists _ (Var ghost _) (Just (witness, substituted)) _)
    = do
    -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run (substitute var witness body) substituted
  run (Forall _ u _) (Forall _ w _) = run u w
  run (One  l      ) (One  l'     ) = return $ One (max l l')
  run (Top  l      ) (Top  l'     ) = return $ Top (max l l')
  run (Zero l      ) (Zero l'     ) = return $ Zero (max l l')
  run (Bot  l      ) (Bot  l'     ) = return $ Bot (max l l')
  run t              v              = throwError (t, v)

-- replace a type variable in some type with another type
substitute :: TypeVar -> Type -> Type -> Type
-- substitute var new (Var (DualOf var') l)
--   | var ==      var' = dual new
--   | otherwise        = Var (DualOf var') l
substitute var new (Var var' l) | var == var' = new
                                | otherwise   = Var var' l
substitute var new (Dual t l) = Dual (substitute var new t) l
substitute var new (Times t u l) =
  Times (substitute var new t) (substitute var new u) l
substitute var new (Par t u l) =
  Par (substitute var new t) (substitute var new u) l
substitute var new (Plus t u l) =
  Plus (substitute var new t) (substitute var new u) l
substitute var new (With t u l) =
  With (substitute var new t) (substitute var new u) l
substitute var new (Acc t l) = Acc (substitute var new t) l
substitute var new (Req t l) = Req (substitute var new t) l
substitute var new (Exists t u Nothing l) =
  Exists t (substitute var new u) Nothing l
substitute var new (Exists t u (Just (v, w)) l) = Exists
  t
  (substitute var new u)
  (Just (substitute var new v, substitute var new w))
  l
substitute var new (Forall t u l) = Forall t (substitute var new u) l
substitute _   _   others         = others


apply :: Substitution -> Type -> Type
apply (Substitute x y) = substitute x y

-- remove recurring Duals
normalize :: Type -> Type
normalize = dual . dual
-- normalize (Dual (Dual t _) _) = t
-- normalize (Times t u l) = Times (normalize t) (normalize u) l
-- normalize (Par t u l) = Par (normalize t) (normalize u) l
-- normalize (Plus t u l) = Plus (normalize t) (normalize u) l
-- normalize (With t u l) = With (normalize t) (normalize u) l
-- normalize (Acc t l) = Acc (normalize t) l
-- normalize (Req t l) = Req (normalize t) l
-- normalize (Exists t u Nothing l) = Exists t (normalize u) Nothing l
-- normalize (Exists t u (Just (v, w)) l) = Exists t (normalize u) (Just (normalize v, normalize w)) l
-- normalize (Forall t u l) = Forall t (normalize u) l
-- normalize others = others
