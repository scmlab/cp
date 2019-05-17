{-# LANGUAGE OverloadedStrings #-}

module Test.TypeChecking where
--
import Control.Arrow ((***))
import Control.Monad.Except
import Data.Text (Text, pack)
import Data.Map (Map)
import Data.Loc (Loc)
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
--
import Test.Util
import Syntax.Abstract
import qualified Syntax.Concrete as C
import qualified Test.TypeChecking.Inference as Inference

import Main

tests :: TestTree
tests = testGroup "Type Checking"
  [ Inference.tests
  ]
