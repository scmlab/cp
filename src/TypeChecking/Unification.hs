module TypeChecking.Unification where

import qualified Syntax.Concrete as C
import Syntax.Concrete hiding (Session(..), Type(..), TypeVar(..))
import qualified Syntax.Abstract as A
import Syntax.Abstract (Session, Type(..), TypeVar(..))
import Syntax.Base

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

--------------------------------------------------------------------------------
-- | Unification

data Substitution = Substitute TypeVar Type
  deriving (Show)
type UniError = (Type, Type)
type UniM = ExceptT UniError (State [Substitution])

unify :: Type -> Type -> (Either (Type, Type) Type, [Substitution])
unify a b = runState (runExceptT (run a b)) []
  where
    run :: Type -> Type -> UniM Type
    run (Var        i)  v               = do
      modify ((:) (Substitute i v))
      return v
    run t               (Var        j)  = do
      modify ((:) (Substitute j t))
      return t
    -- run (Subst  t x u)  (Subst  v y w)  = Subst  <$> run t v <*> (modify ((:) (Substitute x (Var y))) >> return y) <*> run u w
    run (Dual     t  )  v               = dual   <$> run t        (dual v)
    run t               (Dual     v  )  = dual   <$> run (dual t) v
    run (Times    t u)  (Times    v w)  = Times  <$> run t v <*> run u w
    run (Par      t u)  (Par      v w)  = Par    <$> run t v <*> run u w
    run (Plus     t u)  (Plus     v w)  = Plus   <$> run t v <*> run u w
    run (With     t u)  (With     v w)  = With   <$> run t v <*> run u w
    run (Acc      t  )  (Acc      v  )  = run t v
    run (Req      t  )  (Req      v  )  = run t v
    run (Exists   _ u Nothing) (Exists   _ w Nothing)  = run u w
    -- happens when we are composing ∃ with ∀
    run (Exists   _ (Var ghost) (Just (witness, substituted))) (Exists var body Nothing) = do
      -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run substituted (substitute var witness body)
    run (Exists var body Nothing)  (Exists   _ (Var ghost) (Just (witness, substituted))) = do
      -- substitute the ghost of ∃ with the body of ∀
      modify ((:) (Substitute ghost body))
      -- unify the substituted ghost with the substituted body
      run (substitute var witness body) substituted
    run (Forall   _ u)  (Forall   _ w)  = run u w
    run One             One             = return One
    run Top             Top             = return Top
    run Zero            Zero            = return Zero
    run Bot             Bot             = return Bot
    run t               v               = throwError (t, v)

-- replace a type variable in some type with another type
substitute :: TypeVar -> Type -> Type -> Type
substitute var new (Var var')
  | var ==      var' = new
  | otherwise        = Var var'
substitute var new (Dual t)       = Dual (substitute var new t)
substitute var new (Times t u)    = Times (substitute var new t) (substitute var new u)
substitute var new (Par t u)      = Par (substitute var new t) (substitute var new u)
substitute var new (Plus t u)     = Plus (substitute var new t) (substitute var new u)
substitute var new (With t u)     = With (substitute var new t) (substitute var new u)
substitute var new (Acc t)        = Acc (substitute var new t)
substitute var new (Req t)        = Req (substitute var new t)
substitute var new (Exists t u Nothing) = Exists t (substitute var new u) Nothing
substitute var new (Exists t u (Just (v, w))) = Exists t (substitute var new u) (Just (substitute var new v, substitute var new w))
substitute var new (Forall t u)   = Forall t (substitute var new u)
substitute _   _   others         = others


-- freeVariable :: Process Loc -> Set (TermName Loc)
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
