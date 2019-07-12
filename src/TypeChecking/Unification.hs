module TypeChecking.Unification where

import Syntax.Binding (Type(..), TypeVar(..))
-- import Syntax.Abstract (Type(..), TypeVar(..))
import Syntax.Base

import Control.Monad.State
import Control.Monad.Except

--------------------------------------------------------------------------------
-- | Unification

data Substitution = Substitute
                      TypeVar   -- substitutee
                      Type      -- substituter

instance Show Substitution where
  show (Substitute var t) = show var ++ " => " ++ show t

type UniError = (Type, Type)
type UniM = ExceptT UniError (State [Substitution])

unify :: Type -> Type -> (Either (Type, Type) Type, [Substitution])
unify a b = runState (runExceptT (run a b)) []
  where
    run :: Type -> Type -> UniM Type
    run (Var        i _)  v               = do
      modify ((:) (Substitute i v))
      return v
    run t               (Var        j _)  = do
      modify ((:) (Substitute j t))
      return t
    -- run (Subst  t x u)  (Subst  v y w)  = Subst  <$> run t v <*> (modify ((:) (Substitute x (Var y))) >> return y) <*> run u w
    run (Dual     t   _)  v                  = dual   <$> run t        (dual v)
    run t                 (Dual     v   _ )  = dual   <$> run (dual t) v
    run (Times    t u l)  (Times    v w l')  = Times  <$> run t v <*> run u w <*> pure (max l l')
    run (Par      t u l)  (Par      v w l')  = Par    <$> run t v <*> run u w <*> pure (max l l')
    run (Plus     t u l)  (Plus     v w l')  = Plus   <$> run t v <*> run u w <*> pure (max l l')
    run (With     t u l)  (With     v w l')  = With   <$> run t v <*> run u w <*> pure (max l l')
    run (Acc      t   _)  (Acc      v   _ )  = run t v
    run (Req      t   _)  (Req      v   _ )  = run t v
    run (Exists   _ u Nothing _) (Exists   _ w Nothing _)  = run u w
    -- happens when we are composing ∃ with ∀
    run (Exists   _ (Var ghost _) (Just (witness, substituted)) _) (Exists var body Nothing _) = do
      -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run substituted (substitute var witness body)
    run (Exists var body Nothing _)  (Exists   _ (Var ghost _) (Just (witness, substituted)) _) = do
      -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run (substitute var witness body) substituted
    run (Forall   _ u _)    (Forall   _ w _)     = run u w
    run (One l)             (One l')             = return $ One (max l l')
    run (Top l)             (Top l')             = return $ Top (max l l')
    run (Zero l)            (Zero l')            = return $ Zero (max l l')
    run (Bot l)             (Bot l')             = return $ Bot (max l l')
    run t                   v                    = throwError (t, v)

-- replace a type variable in some type with another type
substitute :: TypeVar -> Type -> Type -> Type
-- substitute var new (Var (DualOf var') l)
--   | var ==      var' = dual new
--   | otherwise        = Var (DualOf var') l
substitute var new (Var var' l)
  | var ==      var' = new
  | otherwise        = Var var' l
substitute var new (Dual t l)       = Dual (substitute var new t) l
substitute var new (Times t u l)    = Times (substitute var new t) (substitute var new u) l
substitute var new (Par t u l)      = Par (substitute var new t) (substitute var new u) l
substitute var new (Plus t u l)     = Plus (substitute var new t) (substitute var new u) l
substitute var new (With t u l)     = With (substitute var new t) (substitute var new u) l
substitute var new (Acc t l)        = Acc (substitute var new t) l
substitute var new (Req t l)        = Req (substitute var new t) l
substitute var new (Exists t u Nothing l) = Exists t (substitute var new u) Nothing l
substitute var new (Exists t u (Just (v, w)) l) = Exists t (substitute var new u) (Just (substitute var new v, substitute var new w)) l
substitute var new (Forall t u l)   = Forall t (substitute var new u) l
substitute _   _   others         = others


apply :: Substitution -> Type -> Type
apply (Substitute x y) = substitute x y

-- freeVariable :: Process -> Set Chan
-- freeVariable (Var x _) = Set.fromList [x, y]
-- freeVariable (Link x y _) = Set.fromList [x, y]
-- freeVariable (Compose x _ p q _) = Set.delete x $ Set.union (freeVariable p) (freeVariable q)
-- freeVariable (Output x y p q _) = Set.insert x $ Set.delete y $ Set.union (freeVariable p) (freeVariable q)
-- freeVariable (Input x y p _) = Set.insert x $ Set.delete y (freeVariable p)
-- freeVariable (SelectL x p _) = Set.insert x $ freeVariable p
-- freeVariable (SelectR x p _) = Set.insert x $ freeVariable p
-- freeVariable (Choice x p q _) = Set.insert x $ Set.union (freeVariable p) (freeVariable q)
-- freeVariable (Accept x y p _) = Set.insert x $ Set.delete y (freeVariable p)
-- freeVariable (Request x y p _) = Set.insert x $ Set.delete y (freeVariable p)
-- freeVariable (OutputT x _ p _) = Set.insert x (freeVariable p)
-- freeVariable (InputT x _ p _) = Set.insert x (freeVariable p)
-- freeVariable (EmptyOutput x _) = Set.singleton x
-- freeVariable (EmptyInput x p _) = Set.insert x $ freeVariable p
-- freeVariable (EmptyChoice x _) = Set.singleton x
