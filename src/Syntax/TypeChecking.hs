module Syntax.TypeChecking where

import qualified Syntax.Concrete as C
import Syntax.Concrete (HasDual(..))

import Data.Loc (Loc)
import Data.Function (on)

--------------------------------------------------------------------------------
-- | Types with Variables




data Index = Pos Int | Neg Int
    deriving (Show)

instance C.HasDual Index where
  dual (Pos i) = Neg i
  dual (Neg i) = Pos i

instance Eq Index where
  (==) = (==) `on` C.dual

data Type
    = Var Index
    | Dual Type
    | Times Type Type
    | Par Type Type
    | Plus Type Type
    | With Type Type
    | Acc Type
    | Req Type
    | Exists Int Type
    | Forall Int Type
    | One
    | Bot
    | Zero
    | Top
    deriving (Show)

instance Eq Type where
  Var    i   == Var    j      = i == j
  Dual   a   == Dual   b      = dual a == dual b
  Times  a b == Times  c d    = dual a == dual c && dual b == dual d
  Par    a b == Par    c d    = dual a == dual c && dual b == dual d
  Plus   a b == Plus   c d    = dual a == dual c && dual b == dual d
  With   a b == With   c d    = dual a == dual c && dual b == dual d
  Acc    a   == Acc    b      = dual a == dual b
  Req    a   == Req    b      = dual a == dual b
  Exists x a == Exists y b    = dual a == dual b && x == y
  Forall x a == Forall y b    = dual a == dual b && x == y
  One        == One           = True
  Bot        == Bot           = True
  Zero       == Zero          = True
  Top        == Top           = True
  _          == _             = False

-- no Dual on the RHS
instance HasDual Type where
  dual (Var (Pos n))  = Var (Neg n)
  dual (Var (Neg n))  = Var (Pos n)
  dual (Dual a)       = a
  dual (Times a b)    = Par (dual a) (dual b)
  dual (Par a b)      = Times (dual a) (dual b)
  dual (Plus a b)     = With (dual a) (dual b)
  dual (With a b)     = Plus (dual a) (dual b)
  dual (Acc a)        = Req (dual a)
  dual (Req a)        = Acc (dual a)
  dual (Exists x a)   = Forall x (dual a)
  dual (Forall x a)   = Exists x (dual a)
  dual One            = Bot
  dual Bot            = One
  dual Zero           = Top
  dual Top            = Zero
