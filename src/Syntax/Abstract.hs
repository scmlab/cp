
module Syntax.Abstract where

import Syntax.Base

import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Term level

type TypeName = Text
type TermName = Text

data Program = Program [Declaration]
    deriving (Show)
data Declaration
    = TypeSig  TermName Type
    | TermDefn TermName Process
    deriving (Show)
data Process
    -- link: x ↔ y
    = Link TermName TermName
    -- parallelcomposition: νx:(P|Q)
    | Compose TermName Type Process Process
    -- output: x[y].(P|Q)
    | Output TermName TermName Process Process
    -- input: x(y).P
    | Input TermName TermName Process
    -- left selection: x[inl].P
    | SelectL TermName Process
    -- right selection: x[inr].P
    | SelectR TermName Process
    -- choice: x.case(P,Q)
    | Choice TermName Process Process
    -- server accept: !x(y).P
    | Accept TermName TermName Process
    -- client request: ?x[y].P
    | Request TermName TermName Process
    -- output type: x[Y].P
    | OutputT TermName TypeName Process
    -- input type: x(Y).P
    | InputT TermName TypeName Process
    -- empty output: x[].0
    | EmptyOutput TermName
    -- empty input: x().P
    | EmptyInput TermName Process
    -- empty choice: x.case()
    | EmptyChoice TermName
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Type level

data Type
    = Var TypeVar
    | Dual Type
    -- A ⊗ B: output A then behave as B
    | Times Type Type
    -- A 􏰂􏰂􏰂⅋ B: input A then behave as B
    | Par Type Type
    -- A ⊕ B: select A or B
    | Plus Type Type
    -- A & B: offer choice of A or B
    | With Type Type
    -- of cource!
    | Acc Type
    -- why not?
    | Req Type
    -- existential
    | Exists TermName Type
    -- universal
    | Forall TermName Type
    -- 1: unit for Times ⊗
    | One
    -- ⊥: unit for Par ⅋
    | Bot
    -- 0: unit for Plus ⊕
    | Zero
    -- ⊤: unit for With &
    | Top
    deriving (Eq, Show)

instance HasDual Type where
    dual (Var a)        = Var (dual a)
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
