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
import TypeChecking (inferTerm, inferTerm2)
import Main

tests :: TestTree
tests = testGroup "Type Inference"
  [ basics
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
        [ ("x", Dual (Var (Nameless 1)))
        , ("y",      (Var (Nameless 1)))
        ]
  actual @?= expected

cut :: TestTree
cut = testCase "cut" $ do
  actual <- parseProc "\\x . (x[].end | x().end)" >>= infer
  let expected = Map.fromList []
  actual @?= expected


output :: TestTree
output = testCase "output" $ do
  actual <- parseProc "x[y].(y[].end | x().end)" >>= infer
  let expected = Map.fromList [("x", Times One Bot)]
  actual @?= expected


input :: TestTree
input = testCase "input" $ do
  actual <- parseProc "x(y).(y[].end)" >>= infer
  let expected = Map.fromList
        [ ("x", Par One (Var (Nameless 1)))
        ]
  actual @?= expected

selectLeft :: TestTree
selectLeft = testCase "select left" $ do
  actual <- parseProc "x[inl].x[].end" >>= infer
  let expected = Map.fromList
        [ ("x", Plus One (Var (Nameless 1)))
        ]
  actual @?= expected

selectRight :: TestTree
selectRight = testCase "select right" $ do
  actual <- parseProc "x[inr].x[].end" >>= infer
  let expected = Map.fromList
        [ ("x", Plus (Var (Nameless 1)) One)
        ]
  actual @?= expected

choice :: TestTree
choice = testCase "choice" $ do
  actual <- parseProc "x.case(x[].end, x[].end)" >>= infer
  let expected = Map.fromList
        [ ("x", With One One)
        ]
  actual @?= expected


accept :: TestTree
accept = testCase "accept" $ do
  actual <- parseProc "!x(y).(y[].end)" >>= infer
  let expected = Map.fromList
        [ ("x", Acc One)
        ]
  actual @?= expected

request :: TestTree
request = testCase "request" $ do
  actual <- parseProc "?x[y].y[].end" >>= infer
  let expected = Map.fromList
        [ ("x", Req One)
        ]
  actual @?= expected
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
  let expected = Map.fromList
        [ ("x", Exists Unknown (Var (Nameless 1)) (Just (One, One)))
        ]
  actual @?= expected

inputType :: TestTree
inputType = testCase "input type" $ do
  actual <- parseProc "x(X).x[].end" >>= infer
  let expected = Map.fromList
        [ ("x", Forall (Named "X") One)
        ]
  actual @?= expected

emptyOutput :: TestTree
emptyOutput = testCase "emptyOutput" $ do
  actual <- parseProc "x[].end" >>= infer
  let expected = Map.fromList [("x", One)]
  actual @?= expected

emptyInput :: TestTree
emptyInput = testCase "emptyInput" $ do
  actual <- parseProc "x().end" >>= infer
  let expected = Map.fromList [("x", Bot)]
  actual @?= expected

emptyChoice :: TestTree
emptyChoice = testCase "emptyChoice" $ do
  actual <- parseProc "x.case()" >>= infer
  let expected = Map.fromList [("x", Top)]
  actual @?= expected

-- infer :: C.Process Loc -> IO Session
-- infer term = do
--   (result, _) <- runM $ runTCM (inferTerm term)
--   case result of
--     Left  err         -> assertFailure $ show err
--     Right (actual, _) -> return actual

infer :: C.Process Loc -> IO Session
infer term = do
  (result, _) <- runM $ runTCM (inferTerm2 term)
  case result of
    Left  err         -> assertFailure $ show err
    Right (actual, _) -> return actual
-- tests :: TestTree
-- tests = testGroup "TypeChecker"
--   [ testCase "p0" $ do
--       let expected = Right ()
--       let actual = runExcept $ checkPi [] [] p0
--       actual @?= Left "variable Neg \"c\" not found"
--   , testCase "p1" $ do
--       let expected = Right ()
--       let actual = runExcept $ checkPi [] [] p1
--       actual @?= Left "variable Pos \"c\" not found"
--   , testCase "p2" $ do
--       let expected = Right ()
--       let actual = runExcept $ checkPi [] [] p2
--       actual @?= expected
--   ]
--   where
--     t0 :: SType
--     t0 = tsele [("NEG", tsend TInt  $ trecv TInt  $ TEnd),
--                 ("ID",  tsend TBool $ trecv TBool $ TEnd)]
--
--     t1 :: SType
--     t1 = tRecv t0 TEnd
--
--     senv0 :: SEnv
--     senv0 = [(Pos (pack "c"), t1),
--              (Neg (pack "c"), dual t1),
--              (Pos (pack "d"), t0),
--              (Neg (pack "d"), dual t0)
--              ]
--
--
--     p0 :: Pi
--     p0 = Send (cN "c") (ePtrnVar "d") $
--           choices (cN "d")
--             [("NEG", recv (cN "d") (pn "x") $
--                        Send (cN "d") (eI 0 `ESub` ePtrnVar "x") End),
--              ("ID", recv (cN "d") (pn "x") $
--                         Send (cN "d") (ePtrnVar "x") End)]
--
--     p1 :: Pi
--     p1 = recv (cP "c") (pn "z") $
--           Send (cP "z") (eL "NEG") $
--            Send (cP "z") (eI 3) $
--                recv (cP "z") (pn "w") End
--
--     p2 :: Pi
--     p2 = nu "c" t1 (nu "d" t0 p0 `Par` p1)
