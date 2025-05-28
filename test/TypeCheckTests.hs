module TypeCheckTests where

import Data.Either (isLeft)
import Lang.Abs (Type (..))
import Run (infertype)
import Test.Hspec

tcTest :: String -> Type -> Spec
tcTest input expected =
  it (input ++ " should type check to " ++ show expected) $ do
    infertype input `shouldBe` Right expected

tcErrorTest :: String -> Spec
tcErrorTest input =
  it (input ++ " should not type check") $ do
    infertype input `shouldSatisfy` isLeft

test :: IO ()
test = hspec $ do
  describe "typeChecker: good weather tests" $ do
    tcTest "let x = suc (suc zero) in x + suc (suc (suc zero))" TNat
    tcTest "True && False" TBool

    tcTest "val x = suc (suc (suc zero)); x * x" TNat

    tcTest "fun isZero (x : nat) = x == zero; isZero (suc (suc zero))" TBool

  describe "typeChecker: bad weather tests" $ do
    tcErrorTest "let x = suc (suc zero) in x + True"
    tcErrorTest "True && suc (suc (suc zero))"
    tcErrorTest "fun increment (x : nat) = x + suc zero; increment True"
