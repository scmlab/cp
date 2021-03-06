{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Syntax.Abstract where

import qualified Syntax.Concrete               as C
-- import Base
import           TypeChecking.Base

import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Text                      ( Text )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

--------------------------------------------------------------------------------
-- | Abstract Binding Tree

-- Names
type Name = Text
type TypeName = Text

-- Channel
type Chan = Text

-- Program
type Definitions = Map Name Process

-- Process
data Process
  = Atom      Name (Set Chan)
  | Link      Chan Chan
  | Compose   Chan Process Process
  | Output    Chan Chan Process Process
  | Input     Chan Chan Process
  | SelectL   Chan Process
  | SelectR   Chan Process
  | Choice    Chan Process Process
  | Accept    Chan Chan Process
  | Request   Chan Chan Process
  | OutputT   Chan Process
  | InputT    Chan Process
  | EmptyOutput Chan
  | EmptyInput  Chan Process
  | EmptyChoice Chan
  | End
  | Mix       Process   Process
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- | Instances



--------------------------------------------------------------------------------
-- | Make free variables bounded

subsituteChannel :: Chan -> Chan -> Chan -> Chan
subsituteChannel old new chan = if chan == old then new else chan

subsitute :: Chan -> Chan -> Process -> Process
subsitute old new process = case process of
  Atom name chans -> if Set.member old chans
    then Atom name (Set.insert new (Set.delete old chans))
    else Atom name chans
  Link x y       -> Link (chan x) (chan y)
  Compose x a b  -> Compose (chan x) (subst a) (subst b)
  Output x y a b -> Output (chan x) (chan y) (subst a) (subst b)
  Input x y a    -> Input (chan x) (chan y) (subst a)
  SelectL x a    -> SelectL (chan x) (subst a)
  SelectR x a    -> SelectR (chan x) (subst a)
  Choice  x a b  -> Choice (chan x) (subst a) (subst b)
  Accept  x y a  -> Accept (chan x) (chan y) (subst a)
  Request x y a  -> Request (chan x) (chan y) (subst a)
  OutputT x a    -> OutputT (chan x) (subst a)
  InputT  x a    -> InputT (chan x) (subst a)
  EmptyOutput x  -> EmptyOutput (chan x)
  EmptyInput x a -> EmptyInput (chan x) (subst a)
  EmptyChoice x  -> EmptyChoice (chan x)
  End            -> End
  Mix p q        -> Mix (subst p) (subst q)
 where
  chan  = subsituteChannel old new
  subst = subsitute old new


--------------------------------------------------------------------------------
-- Free variables

freeChans :: Process -> Set Chan
freeChans process = case process of
  Atom _ xs     -> xs
  Link x y      -> Set.fromList [x, y]
  Compose x p q -> Set.delete x $ Set.union (freeChans p) (freeChans q)
  Output x y p q ->
    Set.insert x $ Set.delete y $ Set.union (freeChans p) (freeChans q)
  Input x y p    -> Set.insert x $ Set.delete y (freeChans p)
  SelectL x p    -> Set.insert x $ freeChans p
  SelectR x p    -> Set.insert x $ freeChans p
  Choice  x p q  -> Set.insert x $ Set.union (freeChans p) (freeChans q)
  Accept  x y p  -> Set.insert x $ Set.delete y (freeChans p)
  Request x y p  -> Set.insert x $ Set.delete y (freeChans p)
  OutputT x p    -> Set.insert x (freeChans p)
  InputT  x p    -> Set.insert x (freeChans p)
  EmptyOutput x  -> Set.singleton x
  EmptyInput x p -> Set.insert x (freeChans p)
  EmptyChoice x  -> Set.singleton x
  End            -> Set.empty
  Mix p q        -> Set.union (freeChans p) (freeChans q)

isRequesting :: Chan -> Process -> Maybe Chan
isRequesting chan process = case process of
  Atom _ _       -> Nothing
  Link _ _       -> Nothing
  Compose _ p q  -> isRequesting chan p <> isRequesting chan q
  Output _ _ p q -> isRequesting chan p <> isRequesting chan q
  Input _ _ p    -> isRequesting chan p
  SelectL _ p    -> isRequesting chan p
  SelectR _ p    -> isRequesting chan p
  Choice  _ p q  -> isRequesting chan p <> isRequesting chan q
  Accept  _ _ p  -> isRequesting chan p
  Request x y p  -> if chan == x then Just y else isRequesting chan p
  OutputT _ p    -> isRequesting chan p
  InputT  _ p    -> isRequesting chan p
  EmptyOutput _  -> Nothing
  EmptyInput _ p -> isRequesting chan p
  EmptyChoice _  -> Nothing
  End            -> Nothing
  Mix p q        -> isRequesting chan p <> isRequesting chan q



  --
--   --
--   --   case p of
-- freeChans' :: Proc -> FreeChans
-- freeChans' p = case p of
--------------------------------------------------------------------------------
-- Converting from Concrete Syntax Tree

type AbstractM = ExceptT ScopeError (Reader C.Definitions)

runAbstractM :: C.Definitions -> AbstractM a -> Either ScopeError a
runAbstractM defns f = runReader (runExceptT f) defns

class FromConcrete a b | a -> b where
  fromConcrete :: a -> AbstractM b

instance FromConcrete C.Definition Process where
  fromConcrete (C.Paired _ p _) = fromConcrete p
  fromConcrete (C.TypeOnly n t) =
    Atom
      <$> fromConcrete n
      <*> (Set.fromList <$> mapM fromConcrete (Map.keys t))
  fromConcrete (C.TermOnly _ p) = fromConcrete p

instance FromConcrete C.Definitions Definitions where
  fromConcrete definitions = do
    let pairs = Map.toList definitions
    pairs' <- forM pairs $ \(k, v) -> do
      k' <- fromConcrete k
      v' <- fromConcrete v
      return (k', v')
    return $ Map.fromList pairs'

instance FromConcrete C.Process Process where
  fromConcrete process = case process of
    C.Call name _ -> do
      definitions <- ask
      case Map.lookup name definitions of
        Nothing ->
          error
            "[panic] Definition not found, this shouldn't happen at the syntax tree converting stage"
        Just p -> fromConcrete p

    C.Link x y _ -> Link <$> fromConcrete x <*> fromConcrete y

    C.Compose x _ p q _ ->
      Compose <$> fromConcrete x <*> fromConcrete p <*> fromConcrete q

    C.Output x y p q _ ->
      Output
        <$> fromConcrete x
        <*> fromConcrete y
        <*> fromConcrete p
        <*> fromConcrete q

    C.Input x y p _ ->
      Input <$> fromConcrete x <*> fromConcrete y <*> fromConcrete p

    C.SelectL x p _ -> SelectL <$> fromConcrete x <*> fromConcrete p

    C.SelectR x p _ -> SelectR <$> fromConcrete x <*> fromConcrete p

    C.Choice x p q _ ->
      Choice <$> fromConcrete x <*> fromConcrete p <*> fromConcrete q

    C.Accept x y q _ ->
      Accept <$> fromConcrete x <*> fromConcrete y <*> fromConcrete q

    C.Request x y q _ ->
      Request <$> fromConcrete x <*> fromConcrete y <*> fromConcrete q

    C.OutputT x _ p _  -> OutputT <$> fromConcrete x <*> fromConcrete p

    C.InputT  x _ p _  -> InputT <$> fromConcrete x <*> fromConcrete p

    C.EmptyOutput x _  -> EmptyOutput <$> fromConcrete x

    C.EmptyInput x p _ -> EmptyInput <$> fromConcrete x <*> fromConcrete p

    C.EmptyChoice x _  -> EmptyChoice <$> fromConcrete x

    C.End _            -> return End

    C.Mix p q _        -> Mix <$> fromConcrete p <*> fromConcrete q

instance FromConcrete C.Chan Chan where
  fromConcrete (C.Chan name _) = return name

instance FromConcrete C.Name Name where
  fromConcrete (C.Name name _) = return name


--------------------------------------------------------------------------------
-- | Other forms of Syntax Trees

-- data Z =

  -- -- Process
  -- data Process
  --   = Atom      Name
  --   | Link      Chan Chan
  --   | Compose   Chan Process Process
  --   | Output    Chan Chan Process Process
  --   | Input     Chan Chan Process
  --   | SelectL   Chan Process
  --   | SelectR   Chan Process
  --   | Choice    Chan Process Process
  --   | Accept    Chan Chan Process
  --   | Request   Chan Chan Process
  --   | OutputT   Chan Process
  --   | InputT    Chan Process
  --   | EmptyOutput Chan
  --   | EmptyInput  Chan Process
  --   | EmptyChoice Chan
  --   | End
  --   | Mix       Process   Process
  --   deriving (Eq, Ord, Show)
