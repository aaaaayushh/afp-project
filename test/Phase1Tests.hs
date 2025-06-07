module Phase1Tests where

import Data.Either (isLeft, isRight)
import Lang.Abs (Type (..), Ident (..))
import Run (run, infertype)
import Test.Hspec
import Value qualified as V

-- Helper function for type checking tests
tcTest :: String -> Type -> Spec
tcTest input expected =
  it (input ++ " should type check to " ++ show expected) $ do
    infertype input `shouldBe` Right expected

-- Helper function for type checking error tests
tcErrorTest :: String -> Spec
tcErrorTest input =
  it (input ++ " should not type check") $ do
    infertype input `shouldSatisfy` isLeft

-- Helper function for evaluation tests
interpTest :: String -> V.Value -> Spec
interpTest input expected =
  it (input ++ " should evaluate to " ++ show expected) $ do
    run input `shouldBe` Right expected

-- Helper function for successful evaluation tests (when exact value doesn't matter)
interpSuccessTest :: String -> Spec
interpSuccessTest input =
  it (input ++ " should evaluate successfully") $ do
    run input `shouldSatisfy` isRight

-- Helper function for evaluation error tests
interpErrorTest :: String -> Spec
interpErrorTest input =
  it (input ++ " should not evaluate") $ do
    run input `shouldSatisfy` isLeft

test :: IO ()
test = hspec $ do
  describe "Phase 1: Universe Type" $ do
    tcTest "U" TU
    interpTest "U" V.VU

  describe "Phase 1: Types as Values" $ do
    tcTest "nat" TU
    interpTest "nat" (V.VType TNat)
    tcTest "bool" TU
    interpTest "bool" (V.VType TBool)

  describe "Phase 1: Function Types as Expressions" $ do
    tcTest "nat -> bool" TU
    interpTest "nat -> bool" (V.VType (TFun TNat TBool))
    tcTest "U -> U" TU
    interpTest "U -> U" (V.VType (TFun TU TU))
    tcTest "bool -> nat -> bool" TU
    interpTest "bool -> nat -> bool" (V.VType (TFun TBool (TFun TNat TBool)))

  describe "Phase 1: Dependent Function Types" $ do
    tcTest "(x : nat) -> bool" TU
    interpErrorTest "(x : nat) -> bool"
    tcTest "(x : U) -> nat" TU
    interpErrorTest "(x : U) -> nat"

  describe "Phase 1: Type-Annotated Lambda Expressions" $ do
    -- Identity functions
    interpSuccessTest "\\(x : nat) -> x"
    interpSuccessTest "\\(x : bool) -> x"
    interpSuccessTest "\\(x : U) -> x"

    -- Type-level identity
    interpSuccessTest "\\(t : U) -> t"

    -- Constant functions
    interpSuccessTest "\\(x : nat) -> zero"
    interpSuccessTest "\\(x : U) -> nat"

    -- Type-annotated lambda expressions
    interpSuccessTest "\\(a : U) -> \\(x : bool) -> \\(y : bool) -> x"

  describe "Phase 1: Nested Type-Annotated Lambdas" $ do
    interpSuccessTest "\\(a : U) -> \\(x : nat) -> x"
    interpSuccessTest "\\(a : U) -> \\(b : U) -> a"
    interpSuccessTest "\\(a : U) -> \\(x : bool) -> \\(y : bool) -> x"

  describe "Phase 1: Church Encoding Foundations" $ do
    -- Simplified Church Boolean type (without dependent variables for now)
    tcTest "U -> U -> U -> U" TU
    interpTest "U -> U -> U -> U" (V.VType (TFun TU (TFun TU (TFun TU TU))))

    -- Simplified Church Natural type
    tcTest "U -> (U -> U) -> U -> U" TU
    interpTest "U -> (U -> U) -> U -> U" (V.VType (TFun TU (TFun (TFun TU TU) (TFun TU TU))))

    -- Church-style functions with type annotations
    interpSuccessTest "\\(a : U) -> \\(x : nat) -> \\(y : nat) -> x"
    interpSuccessTest "\\(a : U) -> \\(x : nat) -> \\(y : nat) -> y"
    interpSuccessTest "\\(a : U) -> \\(s : nat -> nat) -> \\(z : nat) -> z"
    interpSuccessTest "\\(a : U) -> \\(s : nat -> nat) -> \\(z : nat) -> s z"

    -- Type-annotated lambda expressions
    interpSuccessTest "\\(a : U) -> \\(s : bool -> bool) -> \\(z : bool) -> z"

  describe "Church Encoding: Boolean Types" $ do
    -- Note: Full dependent variable scoping (a : U) -> a -> a -> a is beyond basic Phase 1
    -- The complex Church types require advanced dependent type variable binding
    -- tcTest "(a : U) -> a -> a -> a" TU
    -- interpTest "(a : U) -> a -> a -> a" V.VU

    -- ctrue and cfalse patterns (simplified with concrete types)
    interpSuccessTest "\\(a : U) -> \\(x : nat) -> \\(y : nat) -> x"
    interpSuccessTest "\\(a : U) -> \\(x : bool) -> \\(y : bool) -> x"

    -- cfalse: \a x y -> y (simplified with type annotations)
    interpSuccessTest "\\(a : U) -> \\(x : nat) -> \\(y : nat) -> y"
    interpSuccessTest "\\(a : U) -> \\(x : bool) -> \\(y : bool) -> y"

  describe "Church Encoding: Natural Number Types" $ do
    -- Note: Full dependent variable scoping (a : U) -> (a -> a) -> a -> a is beyond basic Phase 1
    -- tcTest "(a : U) -> (a -> a) -> a -> a" TU
    -- interpTest "(a : U) -> (a -> a) -> a -> a" V.VU

    -- czero pattern (simplified with concrete types)
    interpSuccessTest "\\(a : U) -> \\(s : nat -> nat) -> \\(z : nat) -> z"
    interpSuccessTest "\\(a : U) -> \\(s : bool -> bool) -> \\(z : bool) -> z"

  describe "Church Encoding: Function Type Expressions" $ do
    -- More complex function type combinations
    tcTest "(nat -> nat) -> nat -> nat" TU
    interpTest "(nat -> nat) -> nat -> nat" (V.VType (TFun (TFun TNat TNat) (TFun TNat TNat)))

    tcTest "(bool -> bool) -> bool -> bool" TU
    interpTest "(bool -> bool) -> bool -> bool" (V.VType (TFun (TFun TBool TBool) (TFun TBool TBool)))

    -- Nested function types
    tcTest "((nat -> nat) -> nat) -> nat -> nat" TU
    interpTest "((nat -> nat) -> nat) -> nat -> nat" (V.VType (TFun (TFun (TFun TNat TNat) TNat) (TFun TNat TNat)))

  describe "Church Encoding: Advanced Lambda Expressions" $ do
    -- Church successor pattern (simplified)
    interpSuccessTest "\\(x : (nat -> nat) -> nat -> nat) -> \\(s : nat -> nat) -> \\(z : nat) -> s z"

    -- Church if-then-else pattern
    interpSuccessTest "\\(a : U) -> \\(b : (nat -> nat -> nat)) -> b"

    -- Higher-order function patterns
    interpSuccessTest "\\(f : nat -> nat) -> \\(x : nat) -> f x"
    interpSuccessTest "\\(f : bool -> bool) -> \\(x : bool) -> f x"

  describe "Church Encoding: Complex Nested Types" $ do
    -- Church Boolean with generic types
    tcTest "(U -> U -> U) -> U" TU
    interpTest "(U -> U -> U) -> U" (V.VType (TFun (TFun TU (TFun TU TU)) TU))

    -- Church Natural with function composition
    tcTest "((U -> U) -> U -> U) -> U" TU
    interpTest "((U -> U) -> U -> U) -> U" (V.VType (TFun (TFun (TFun TU TU) (TFun TU TU)) TU))

    -- Triple-nested function types
    tcTest "(((U -> U) -> U) -> U) -> U" TU
    interpTest "(((U -> U) -> U) -> U) -> U" (V.VType (TFun (TFun (TFun (TFun TU TU) TU) TU) TU))

  describe "Phase 1: Backward Compatibility" $ do
    -- Ensure Phase 0 features still work
    tcTest "suc zero" TNat
    tcTest "True && False" TBool
    tcTest "\\(x : nat) -> x + zero" (TDepFun (Ident "x") TNat TNat)

    interpTest "suc (suc zero)" (V.VNat (V.Suc (V.Suc V.Zero)))
    interpTest "True && False" (V.VBool False)
    interpSuccessTest "\\(x : nat) -> x + zero"

  describe "Phase 1: Type System Correctness" $ do
    -- Universe hierarchy
    tcTest "U" TU

    -- Types have type U
    tcTest "nat" TU
    tcTest "bool" TU
    tcTest "U -> U" TU
    tcTest "(x : nat) -> bool" TU

    -- Lambda type inference requires annotations
    tcErrorTest "\\x -> x"
    tcErrorTest "\\x -> \\y -> x"

  describe "Phase 1: Error Cases" $ do
    -- Type errors
    tcErrorTest "U + zero"
    tcErrorTest "nat && bool"

    -- Evaluation errors for invalid applications
    interpErrorTest "zero zero"
    interpErrorTest "True False"