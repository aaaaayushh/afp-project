module InterpTests where

import Run (run)
import Test.Hspec
import Value

interpTest :: String -> Value -> Spec
interpTest input expected =
  it (input ++ " should be " ++ show expected) $ do
    run input `shouldBe` Right expected

-- -- Helper function to convert integers to Nat
-- toNat :: Integer -> Nat
-- toNat 0 = Zero
-- toNat n = Suc (toNat (n - 1))

-- Helper function to create VNat values easily
nat :: Integer -> Value
nat = VNat . toNat

test :: IO ()
test = hspec $ do
  describe "Interpreter: good weather tests" $ do
    interpTest "let x = suc (suc zero) in x + suc (suc (suc zero))" (nat 5)
    interpTest "True && False" (VBool False)

    interpTest "val x = suc (suc (suc zero)); x * x" (nat 9)
    interpTest "fun square (x : nat) = x * x; square (suc (suc (suc zero)))" (nat 9)

  describe "Interpreter: order of operation" $ do
    it "multiplication should come before addition" $ do
      let result = run "suc (suc zero) + suc (suc (suc zero)) * suc (suc (suc (suc zero)))"
      result `shouldBe` Right (nat 14)
      run "suc (suc (suc (suc zero))) * suc (suc (suc zero)) + suc (suc zero)" `shouldBe` result

    it "brackets should have the highest priority" $ do
      run "(suc (suc zero) + suc (suc (suc zero))) * suc (suc (suc (suc zero)))" `shouldBe` Right (nat 20)
