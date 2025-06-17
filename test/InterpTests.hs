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
  describe "Interpreter: basic arithmetic" $ do
    interpTest "zero + zero" (nat 0)
    interpTest "suc zero + zero" (nat 1)
    interpTest "zero + suc zero" (nat 1)
    interpTest "suc (suc zero) + suc (suc (suc zero))" (nat 5)
    interpTest "suc (suc (suc zero)) * suc (suc zero)" (nat 6)
    interpTest "zero * suc (suc (suc zero))" (nat 0)
    interpTest "suc (suc (suc zero)) * zero" (nat 0)

  describe "Interpreter: boolean operations" $ do
    interpTest "True && False" (VBool False)
    interpTest "True && True" (VBool True)
    interpTest "False && False" (VBool False)
    interpTest "False && True" (VBool False)
    interpTest "True || False" (VBool True)
    interpTest "True || True" (VBool True)
    interpTest "False || False" (VBool False)
    interpTest "False || True" (VBool True)
    interpTest "!True" (VBool False)
    interpTest "!False" (VBool True)
    interpTest "!(True && False)" (VBool True)
    interpTest "!True || False" (VBool False)

  describe "Interpreter: comparison operations" $ do
    interpTest "zero == zero" (VBool True)
    interpTest "suc zero == suc zero" (VBool True)
    interpTest "zero == suc zero" (VBool False)
    interpTest "suc (suc zero) == suc zero" (VBool False)
    interpTest "zero < suc zero" (VBool True)
    interpTest "suc zero < zero" (VBool False)
    interpTest "suc zero < suc zero" (VBool False)
    interpTest "suc (suc zero) < suc (suc (suc zero))" (VBool True)

  describe "Interpreter: conditional expressions" $ do
    interpTest "if True then suc zero else zero" (nat 1)
    interpTest "if False then suc zero else zero" (nat 0)
    interpTest "if zero == zero then suc (suc zero) else suc zero" (nat 2)
    interpTest "if suc zero < zero then suc zero else suc (suc zero)" (nat 2)
    interpTest "if True && False then suc zero else suc (suc (suc zero))" (nat 3)

  describe "Interpreter: let expressions" $ do
    interpTest "let x = suc (suc zero) in x + suc (suc (suc zero))" (nat 5)
    interpTest "let x = suc zero in let y = suc (suc zero) in x + y" (nat 3)
    interpTest "let x = True in let y = False in x && y" (VBool False)
    interpTest "let x = suc (suc zero) in x * x + x" (nat 6)
    interpTest "let f = \\(n : nat) -> n + suc zero in f (suc (suc zero))" (nat 3)

  describe "Interpreter: function definitions and calls" $ do
    interpTest "val x = suc (suc (suc zero)); x * x" (nat 9)
    interpTest "fun square (x : nat) = x * x; square (suc (suc (suc zero)))" (nat 9)
    interpTest "fun addOne (x : nat) = x + suc zero; addOne (suc (suc zero))" (nat 3)
    interpTest "fun isZero (x : nat) = x == zero; isZero zero" (VBool True)
    interpTest "fun isZero (x : nat) = x == zero; isZero (suc zero)" (VBool False)
    interpTest "fun apply (f : nat -> nat) = f (suc (suc zero)); apply (\\(n : nat) -> n * n)" (nat 4)

  describe "Interpreter: lambda expressions" $ do
    interpTest "(\\(n : nat) -> n + suc zero) (suc (suc zero))" (nat 3)
    interpTest "(\\(x : nat) -> x * x) (suc (suc (suc zero)))" (nat 9)
    interpTest "let b = True in let f = \\(x : bool) -> !x in f b" (VBool False)
    interpTest "let b = False in let f = \\(x : bool) -> !x in f b" (VBool True)

  describe "Interpreter: complex expressions" $ do
    interpTest "let x = suc (suc zero) in let y = suc (suc (suc zero)) in if x < y then x * y else y * x" (nat 6)
    interpTest "let double = \\(x : nat) -> x + x in double (suc (suc zero))" (nat 4)
    interpTest "let addTwo = \\(x : nat) -> x + suc (suc zero) in addTwo (suc zero)" (nat 3)
    interpTest "fun apply (f : nat -> nat) = f (suc (suc zero)); apply (\\(n : nat) -> n + suc zero)" (nat 3)

  describe "Interpreter: order of operations" $ do
    it "multiplication should come before addition" $ do
      let result = run "suc (suc zero) + suc (suc (suc zero)) * suc (suc (suc (suc zero)))"
      result `shouldBe` Right (nat 14)
      run "suc (suc (suc (suc zero))) * suc (suc (suc zero)) + suc (suc zero)" `shouldBe` result

    it "brackets should have the highest priority" $ do
      run "(suc (suc zero) + suc (suc (suc zero))) * suc (suc (suc (suc zero)))" `shouldBe` Right (nat 20)

    it "logical AND should come before OR" $ do
      run "True || False && False" `shouldBe` Right (VBool True)
      run "(True || False) && False" `shouldBe` Right (VBool False)

  describe "Interpreter: edge cases" $ do
    interpTest "zero" (nat 0)
    interpTest "suc (suc (suc (suc (suc zero))))" (nat 5)
    interpTest "True" (VBool True)
    interpTest "False" (VBool False)
    interpTest "let x = zero in x" (nat 0)
    interpTest "fun id (x : nat) = x; id zero" (nat 0)
    interpTest "if True then True else False" (VBool True)
    interpTest "if False then True else False" (VBool False)
