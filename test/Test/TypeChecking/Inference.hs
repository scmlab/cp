{-# LANGUAGE OverloadedStrings #-}

module Test.TypeChecking.Inference where
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
import Syntax.Base
import qualified Syntax.Concrete as C
import qualified TypeChecking.InferOld as Old
import qualified TypeChecking.Infer as New
import TypeChecking (checkAll)
import TypeChecking.Base

import Base
import Main

infer :: C.Process -> IO Session
infer term = do
  (result, _) <- runM $ runTCM (New.inferTerm term)
  case result of
    Left  err         -> assertFailure $ show err
    Right (actual, _) -> return actual

load :: FilePath -> IO (Map Proc Session)
load filePath = do
  (result, _) <- runM $ do
      program <- readSource filePath >>= parseSource filePath
      runTCM (checkAll program)
  case result of
    Left  err         -> assertFailure $ show err
    Right (actual, _) -> return $ Map.mapKeys C.toAbstract actual

defn :: FilePath -> Text -> IO Session
defn filePath name = do
  defns <- load filePath
  case Map.lookup name defns of
    Nothing -> assertFailure $ "Definition not found"
    Just session -> return session

tests :: TestTree
tests = testGroup "Type Inference"
  -- [ examples ]
  [ basics
  , examples
  ]

basics :: TestTree
basics = testGroup "basics"
  [ link
  , cut
  , output
  , input
  , selectLeft
  , selectRight
  , choice
  , accept
  , request
  , outputType
  , inputType
  , emptyOutput
  , emptyInput
  , emptyChoice
  ]

link :: TestTree
link = testCase "link" $ do
  actual <- parseProc "x <-> y" >>= infer
  let expected = Map.fromList
        [ ("x", (Var (DualOf (Nameless 1))))
        , ("y", (Var (Nameless 1)))
        ]
  actual @?= expected

cut :: TestTree
cut = testCase "cut" $ do
  actual <- parseProc "\\x . (x[].end | x().end)" >>= infer
  actual @?= Map.fromList []


output :: TestTree
output = testCase "output" $ do
  actual <- parseProc "x[y].(y[].end | x().end)" >>= infer
  actual @?= Map.fromList [("x", Times One Bot)]


input :: TestTree
input = testCase "input" $ do
  actual <- parseProc "x(y).(y[].end)" >>= infer
  actual @?= Map.fromList
        [ ("x", Par One (Var (Nameless 1)))
        ]

selectLeft :: TestTree
selectLeft = testCase "select left" $ do
  actual <- parseProc "x[inl].x[].end" >>= infer
  actual @?= Map.fromList
        [ ("x", Plus One (Var (Nameless 1)))
        ]

selectRight :: TestTree
selectRight = testCase "select right" $ do
  actual <- parseProc "x[inr].x[].end" >>= infer
  actual @?= Map.fromList
        [ ("x", Plus (Var (Nameless 1)) One)
        ]

choice :: TestTree
choice = testCase "choice" $ do
  actual <- parseProc "x.case(x[].end, x[].end)" >>= infer
  actual @?= Map.fromList
        [ ("x", With One One)
        ]


accept :: TestTree
accept = testCase "accept" $ do
  actual <- parseProc "!x(y).(y[].end)" >>= infer
  actual @?= Map.fromList
        [ ("x", Acc One)
        ]

request :: TestTree
request = testCase "request" $ do
  actual <- parseProc "?x[y].y[].end" >>= infer
  actual @?= Map.fromList
        [ ("x", Req One)
        ]
--
-- weaken :: TestTree
-- weaken = testCase "weaken" $ do
--   actual <- parseProc "x[].end" >>= infer
--   let expected = Map.fromList
--         [ ("x", One)
--         , ("y", Req One)
--         ]
--   actual @?= expected

outputType :: TestTree
outputType = testCase "output type" $ do
  actual <- parseProc "x[1].x[].end" >>= infer
  actual @?= Map.fromList
        [ ("x", Exists Unknown (Var (Nameless 1)) (Just (One, One)))
        ]

inputType :: TestTree
inputType = testCase "input type" $ do
  actual <- parseProc "x(X).x[].end" >>= infer
  actual @?= Map.fromList
        [ ("x", Forall (Named "X") One)
        ]

emptyOutput :: TestTree
emptyOutput = testCase "emptyOutput" $ do
  actual <- parseProc "x[].end" >>= infer
  actual @?= Map.fromList [("x", One)]

emptyInput :: TestTree
emptyInput = testCase "emptyInput" $ do
  actual <- parseProc "x().end" >>= infer
  actual @?= Map.fromList [("x", Bot)]

emptyChoice :: TestTree
emptyChoice = testCase "emptyChoice" $ do
  actual <- parseProc "x.case()" >>= infer
  actual @?= Map.fromList [("x", Top)]

examples :: TestTree
examples = testGroup "examples"
  [ buySell
  , shopQuote
  , selectChoice
  , clientServer
  , churchNumeral
  ]

--
-- debug :: TestTree
-- debug = testCase "debug" $ do
--   main <- get "main"
--   main @?= Map.fromList []
--
--   where
--     get = defn "test/source/a.clp"

whatever :: Type
whatever = Var Unknown

typeOfBuy :: Type
typeOfBuy = Times One (Times One (Par Bot Bot))

buySell :: TestTree
buySell = testCase "buy/sell" $ do
  buy <- get "buy"
  buy @?= Map.fromList [ ("x", typeOfBuy) ]

  sell <- get "sell"
  sell @?= Map.fromList [ ("x", Par Bot (Par Bot (Times One One))) ]

  where
    get = defn "test/source/buy-sell.clp"

typeOfShop :: Type
typeOfShop = Times One (Par Bot whatever)

typeOfShop' :: Type
typeOfShop' = Times One (Par Bot Bot)


shopQuote :: TestTree
shopQuote = testCase "shop/quote" $ do
  shop <- get "shop"
  shop @?= Map.fromList [ ("x", typeOfShop) ]

  quote <- get "quote"
  quote @?= Map.fromList [ ("x", Par Bot (Times One One)) ]

  where
    get = defn "test/source/buy-sell.clp"

selectChoice :: TestTree
selectChoice = testCase "select/choice" $ do
  selectBuy <- get "selectBuy"
  selectBuy @?= Map.fromList [ ("x", Plus typeOfBuy whatever) ]

  selectShop <- get "selectShop"
  selectShop @?= Map.fromList [ ("x", Plus whatever typeOfShop) ]

  choice <- get "choice"
  choice @?= Map.fromList [ ("x", With (dual typeOfBuy) (dual typeOfShop')) ]

  runBuy <- get "runBuy"
  runBuy @?= Map.fromList []

  runShop <- get "runShop"
  runShop @?= Map.fromList []

  where
    get = defn "test/source/buy-sell.clp"

clientServer :: TestTree
clientServer = testCase "client/server" $ do
  client <- get "client"
  client @?= Map.fromList [ ("x", Req whatever) ]

  server <- get "server"
  server @?= Map.fromList [ ("x", Acc whatever) ]


  run <- get "run"
  run @?= Map.fromList []


  where
    get = defn "test/source/buy-sell.clp"

churchNumeral :: TestTree
churchNumeral = testCase "church numeral" $ do

  zero <- get "zero"
  zero @?= Map.fromList [("x", Forall (Named "X") (Par whatever (Par whatever whatever)))]

  one <- get "one"
  one @?= Map.fromList [("x", Forall (Named "X") (Par (Req (Times whatever whatever)) (Par whatever whatever)))]

  where
    get = defn "test/source/church.clp"
