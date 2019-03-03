{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Syntax.Abstract where

import qualified Syntax.Concrete as C
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
    -- parallelcomposition: νx.(P|Q)
    | Compose TermName Process Process
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
    = Dual Type
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
    deriving (Show)

dual :: Type -> Type
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

--------------------------------------------------------------------------------
-- | Converting from Concrete Syntax Tree

class FromConcrete a b | a -> b where
    fromConcrete :: a -> b

instance FromConcrete (C.Program ann) Program where
    fromConcrete (C.Program  declarations _) =
        Program (map fromConcrete declarations)

instance FromConcrete (C.Declaration ann) Declaration where
    fromConcrete (C.TypeSig name typ _) =
        TypeSig (fromConcrete name) (fromConcrete typ)
    fromConcrete (C.TermDefn name process _) =
        TermDefn (fromConcrete name) (fromConcrete process)

instance FromConcrete (C.TypeName ann) TypeName where
    fromConcrete (C.TypeName name    _) = name

instance FromConcrete (C.TermName ann) TermName where
    fromConcrete (C.TermName name    _) = name

instance FromConcrete (C.Process ann) Process where
    fromConcrete (C.Link nameA nameB _) =
        Link
            (fromConcrete nameA)
            (fromConcrete nameB)
    fromConcrete (C.Compose name procA procB _) =
        Compose
            (fromConcrete name)
            (fromConcrete procA)
            (fromConcrete procB)
    fromConcrete (C.Output nameA nameB procA procB _) =
        Output
            (fromConcrete nameA)
            (fromConcrete nameB)
            (fromConcrete procA)
            (fromConcrete procB)
    fromConcrete (C.Input nameA nameB proc _) =
        Input
            (fromConcrete nameA)
            (fromConcrete nameB)
            (fromConcrete proc)
    fromConcrete (C.SelectL name proc _) =
        SelectL
            (fromConcrete name)
            (fromConcrete proc)
    fromConcrete (C.SelectR name proc _) =
        SelectR
            (fromConcrete name)
            (fromConcrete proc)
    fromConcrete (C.Choice name procA procB _) =
        Choice
            (fromConcrete name)
            (fromConcrete procA)
            (fromConcrete procB)
    fromConcrete (C.Accept nameA nameB proc _) =
        Accept
            (fromConcrete nameA)
            (fromConcrete nameB)
            (fromConcrete proc)
    fromConcrete (C.Request nameA nameB proc _) =
        Request
            (fromConcrete nameA)
            (fromConcrete nameB)
            (fromConcrete proc)
    fromConcrete (C.EmptyOutput name _) =
        EmptyOutput
            (fromConcrete name)
    fromConcrete (C.EmptyInput name proc _) =
        EmptyInput
            (fromConcrete name)
            (fromConcrete proc)
    fromConcrete (C.EmptyChoice name _) =
        EmptyChoice
            (fromConcrete name)

instance FromConcrete (C.Type ann) Type where
    fromConcrete (C.Dual t _) =
        Dual
            (fromConcrete t)
    fromConcrete (C.Times t u _) =
        Times
            (fromConcrete t)
            (fromConcrete u)
    fromConcrete (C.Par t u _) =
        Par
            (fromConcrete t)
            (fromConcrete u)
    fromConcrete (C.Plus t u _) =
        Plus
            (fromConcrete t)
            (fromConcrete u)
    fromConcrete (C.With t u _) =
        With
            (fromConcrete t)
            (fromConcrete u)
    fromConcrete (C.Acc t _) =
        Acc
            (fromConcrete t)
    fromConcrete (C.Req t _) =
        Req
            (fromConcrete t)
    fromConcrete (C.Exists x t _) =
        Exists
            (fromConcrete x)
            (fromConcrete t)
    fromConcrete (C.Forall x t _) =
        Forall
            (fromConcrete x)
            (fromConcrete t)
    fromConcrete (C.One _) = One
    fromConcrete (C.Bot _) = Bot
    fromConcrete (C.Zero _) = Zero
    fromConcrete (C.Top _) = Top
