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
import TypeChecking (inferTerm)
import Main

tests :: TestTree
tests = testGroup "TypeChecker"
  [ emptyOutput
  , emptyInput
  , emptyChoice
  ]

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

infer :: C.Process Loc -> IO Session
infer term = do
  (result, _) <- runM $ runTCM (inferTerm term)
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
