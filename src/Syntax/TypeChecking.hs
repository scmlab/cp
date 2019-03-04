module Syntax.TypeChecking where

import qualified Syntax.Concrete as C

import Data.Loc (Loc)

--------------------------------------------------------------------------------
-- | Types with Variables


-- data AssumedType
--
-- data Type
--     =
--          -- Var Index
--     = Dual Type
--     | Times Type Type
--     | Par Type Type
--     | Plus Type Type
--     | With Type Type
--     | Acc Type
--     | Req Type
--     | Exists Int Type
--     | Forall Int Type
--     | One
--     | Bot
--     | Zero
--     | Top
--     deriving (Show)
--
-- dual :: Type -> Type
-- -- dual (Var (Pos n))  = Var (Neg n)
-- -- dual (Var (Neg n))  = Var (Pos n)
-- dual (Dual a)       = a
-- dual (Times a b)    = Par (dual a) (dual b)
-- dual (Par a b)      = Times (dual a) (dual b)
-- dual (Plus a b)     = With (dual a) (dual b)
-- dual (With a b)     = Plus (dual a) (dual b)
-- dual (Acc a)        = Req (dual a)
-- dual (Req a)        = Acc (dual a)
-- dual (Exists x a)   = Forall x (dual a)
-- dual (Forall x a)   = Exists x (dual a)
-- dual One            = Bot
-- dual Bot            = One
-- dual Zero           = Top
-- dual Top            = Zero
