module Syntax.TypeChecking where

import qualified Syntax.Concrete as C

--------------------------------------------------------------------------------
-- | Types with Variables

data Type
    = Var Int
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
