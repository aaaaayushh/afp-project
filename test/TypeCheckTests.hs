module TypeCheckTests where

import Data.Either (isLeft)
import Lang.Abs (Ident (..), Type (..))
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
  describe "typeChecker: basic types" $ do
    tcTest "zero" TNat
    tcTest "suc zero" TNat
    tcTest "suc (suc (suc zero))" TNat
    tcTest "True" TBool
    tcTest "False" TBool

  describe "typeChecker: arithmetic operations" $ do
    tcTest "zero + zero" TNat
    tcTest "suc zero + zero" TNat
    tcTest "zero + suc zero" TNat
    tcTest "suc (suc zero) + suc (suc (suc zero))" TNat
    tcTest "suc (suc (suc zero)) * suc (suc zero)" TNat
    tcTest "zero * suc (suc (suc zero))" TNat
    tcTest "suc (suc (suc zero)) * zero" TNat

  describe "typeChecker: boolean operations" $ do
    tcTest "True && False" TBool
    tcTest "True && True" TBool
    tcTest "False && False" TBool
    tcTest "True || False" TBool
    tcTest "False || True" TBool
    tcTest "!True" TBool
    tcTest "!False" TBool
    tcTest "!(True && False)" TBool
    tcTest "!True || False" TBool

  describe "typeChecker: comparison operations" $ do
    tcTest "zero == zero" TBool
    tcTest "suc zero == suc zero" TBool
    tcTest "zero == suc zero" TBool
    tcTest "suc (suc zero) == suc zero" TBool
    tcTest "zero < suc zero" TBool
    tcTest "suc zero < zero" TBool
    tcTest "suc zero < suc zero" TBool
    tcTest "suc (suc zero) < suc (suc (suc zero))" TBool

  describe "typeChecker: conditional expressions" $ do
    tcTest "if True then suc zero else zero" TNat
    tcTest "if False then suc zero else zero" TNat
    tcTest "if zero == zero then suc (suc zero) else suc zero" TNat
    tcTest "if suc zero < zero then suc zero else suc (suc zero)" TNat
    tcTest "if True && False then suc zero else suc (suc (suc zero))" TNat
    tcTest "if True then True else False" TBool
    tcTest "if False then False else True" TBool

  describe "typeChecker: let expressions" $ do
    tcTest "let x = suc (suc zero) in x + suc (suc (suc zero))" TNat
    tcTest "let x = suc zero in let y = suc (suc zero) in x + y" TNat
    tcTest "let x = True in let y = False in x && y" TBool
    tcTest "let x = suc (suc zero) in x * x + x" TNat
    tcTest "let f = \\(n : nat) -> n + suc zero in f (suc (suc zero))" TNat
    tcTest "let b = True in let f = \\(x : bool) -> !x in f b" TBool

  describe "typeChecker: function definitions and calls" $ do
    tcTest "val x = suc (suc (suc zero)); x * x" TNat
    tcTest "fun square (x : nat) = x * x; square (suc (suc (suc zero)))" TNat
    tcTest "fun addOne (x : nat) = x + suc zero; addOne (suc (suc zero))" TNat
    tcTest "fun isZero (x : nat) = x == zero; isZero zero" TBool
    tcTest "fun isZero (x : nat) = x == zero; isZero (suc zero)" TBool
    tcTest "fun apply (f : nat -> nat) = f (suc (suc zero)); apply (\\(n : nat) -> n * n)" TNat

  describe "typeChecker: lambda expressions" $ do
    tcTest "(\\(n : nat) -> n + suc zero) (suc (suc zero))" TNat
    tcTest "(\\(x : nat) -> x * x) (suc (suc (suc zero)))" TNat
    tcTest "\\(n : nat) -> n + suc zero" (TDepFun (Ident "x") TNat TNat)
    tcTest "\\(x : nat) -> x * x" (TDepFun (Ident "x") TNat TNat)
    tcTest "\\(b : bool) -> !b" (TDepFun (Ident "x") TBool TBool)
    tcTest "\\(x : nat) -> \\(y : nat) -> x + y" (TDepFun (Ident "x") TNat (TDepFun (Ident "x") TNat TNat))

  describe "typeChecker: complex expressions" $ do
    tcTest "let x = suc (suc zero) in let y = suc (suc (suc zero)) in if x < y then x * y else y * x" TNat
    tcTest "let double = \\(x : nat) -> x + x in double (suc (suc zero))" TNat
    tcTest "let addTwo = \\(x : nat) -> x + suc (suc zero) in addTwo (suc zero)" TNat
    tcTest "fun apply (f : nat -> nat) = f (suc (suc zero)); apply (\\(n : nat) -> n + suc zero)" TNat

  describe "typeChecker: precedence and grouping" $ do
    tcTest "suc (suc zero) + suc (suc (suc zero)) * suc (suc (suc (suc zero)))" TNat
    tcTest "(suc (suc zero) + suc (suc (suc zero))) * suc (suc (suc (suc zero)))" TNat
    tcTest "True || False && False" TBool
    tcTest "(True || False) && False" TBool

  describe "typeChecker: good weather tests" $ do
    tcTest "let x = suc (suc zero) in x + suc (suc (suc zero))" TNat
    tcTest "True && False" TBool
    tcTest "val x = suc (suc (suc zero)); x * x" TNat
    tcTest "fun isZero (x : nat) = x == zero; isZero (suc (suc zero))" TBool

  describe "typeChecker: type mismatch errors" $ do
    tcErrorTest "let x = suc (suc zero) in x + True"
    tcErrorTest "True && suc (suc (suc zero))"
    tcErrorTest "fun increment (x : nat) = x + suc zero; increment True"
    tcErrorTest "zero && False"
    tcErrorTest "True + suc zero"
    tcErrorTest "suc zero || True"
    tcErrorTest "!suc zero"
    tcErrorTest "!zero"

  describe "typeChecker: comparison type errors" $ do
    tcErrorTest "True == suc zero"
    tcErrorTest "suc zero == False"
    tcErrorTest "True < False"
    tcErrorTest "zero < True"
    tcErrorTest "False < suc zero"

  describe "typeChecker: conditional type errors" $ do
    tcErrorTest "if suc zero then True else False"
    tcErrorTest "if True then suc zero else False"
    tcErrorTest "if True then True else suc zero"
    tcErrorTest "if zero then suc zero else suc (suc zero)"

  describe "typeChecker: function application errors" $ do
    tcErrorTest "zero zero"
    tcErrorTest "True False"
    tcErrorTest "suc zero suc zero"
    tcErrorTest "(\\(x : nat) -> x) True"
    tcErrorTest "(\\(x : bool) -> x) zero"

  describe "typeChecker: let binding errors" $ do
    tcErrorTest "let x = True in x + suc zero"
    tcErrorTest "let x = suc zero in x && True"
    tcErrorTest "let f = \\(x : nat) -> x in f True"

  describe "typeChecker: lambda type errors" $ do
    tcErrorTest "\\x -> x" -- missing type annotation
    tcErrorTest "\\x -> \\y -> x" -- missing type annotations
  describe "typeChecker: edge cases" $ do
    tcTest "let x = zero in x" TNat
    tcTest "fun id (x : nat) = x; id zero" TNat
    tcTest "fun constTrue (x : nat) = True; constTrue zero" TBool
    tcTest "if True then True else False" TBool
    tcTest "if False then True else False" TBool
