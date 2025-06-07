module Phase2Tests where

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
  describe "Phase 2: Unit Type (Top)" $ do
    tcTest "Top" TU
    interpTest "Top" V.VTop

  describe "Phase 2: Unit Element (tt)" $ do
    tcTest "tt" TTop
    interpTest "tt" V.VTt

  describe "Phase 2: Empty Type (Bot)" $ do
    tcTest "Bot" TU
    interpTest "Bot" V.VBot

  describe "Phase 2: Pair Types" $ do
    tcTest "[nat, bool]" TU
    interpTest "[nat, bool]" V.VU
    tcTest "[Top, Bot]" TU
    interpTest "[Top, Bot]" V.VU
    tcTest "[U, U]" TU
    interpTest "[U, U]" V.VU

  describe "Phase 2: Pair Construction" $ do
    tcTest "(zero, True)" (TPair TNat TBool)
    interpTest "(zero, True)" (V.VPair (V.VNat V.Zero) (V.VBool True))
    tcTest "(tt, zero)" (TPair TTop TNat)
    interpTest "(tt, zero)" (V.VPair V.VTt (V.VNat V.Zero))
    tcTest "(True, False)" (TPair TBool TBool)
    interpTest "(True, False)" (V.VPair (V.VBool True) (V.VBool False))

  describe "Phase 2: Pair Projections" $ do
    tcTest "fst (zero, True)" TNat
    interpTest "fst (zero, True)" (V.VNat V.Zero)
    tcTest "snd (zero, True)" TBool
    interpTest "snd (zero, True)" (V.VBool True)
    tcTest "fst (tt, zero)" TTop
    interpTest "fst (tt, zero)" V.VTt
    tcTest "snd (tt, zero)" TNat
    interpTest "snd (tt, zero)" (V.VNat V.Zero)

  describe "Phase 2: Nested Pairs" $ do
    tcTest "((zero, True), tt)" (TPair (TPair TNat TBool) TTop)
    interpTest "((zero, True), tt)" (V.VPair (V.VPair (V.VNat V.Zero) (V.VBool True)) V.VTt)
    tcTest "fst (fst ((zero, True), tt))" TNat
    interpTest "fst (fst ((zero, True), tt))" (V.VNat V.Zero)
    tcTest "snd (fst ((zero, True), tt))" TBool
    interpTest "snd (fst ((zero, True), tt))" (V.VBool True)

  describe "Phase 2: Boolean Eliminator" $ do
    -- Simple test: elimBool (\x -> nat) zero (suc zero) True should return zero
    tcTest "elimBool (\\(x : bool) -> nat) zero (suc zero) True" TNat
    interpTest "elimBool (\\(x : bool) -> nat) zero (suc zero) True" (V.VNat V.Zero)
    
    -- elimBool (\x -> nat) zero (suc zero) False should return suc zero
    tcTest "elimBool (\\(x : bool) -> nat) zero (suc zero) False" TNat
    interpTest "elimBool (\\(x : bool) -> nat) zero (suc zero) False" (V.VNat (V.Suc V.Zero))

    -- Boolean eliminator with pair types
    tcTest "elimBool (\\(x : bool) -> [nat, bool]) (zero, True) (suc zero, False) True" (TPair TNat TBool)
    interpTest "elimBool (\\(x : bool) -> [nat, bool]) (zero, True) (suc zero, False) True" (V.VPair (V.VNat V.Zero) (V.VBool True))

  describe "Phase 2: Magic Function Type" $ do
    -- Magic function should type check when checked against expected type
    -- Note: magic cannot be easily tested for inference, as it needs Bot values
    interpSuccessTest "\\(x : Top) -> x" -- Just to test that our basic lambda typing still works

  describe "Phase 2: Complex Type Expressions" $ do
    tcTest "[nat -> bool, Top]" TU
    interpTest "[nat -> bool, Top]" V.VU
    tcTest "([nat, bool] -> Top)" TU
    interpTest "([nat, bool] -> Top)" V.VU
    tcTest "(x : [nat, bool]) -> Top" TU
    interpTest "(x : [nat, bool]) -> Top" V.VU

  describe "Phase 2: Functions with New Types" $ do
    interpSuccessTest "\\(x : Top) -> x"
    interpSuccessTest "\\(x : [nat, bool]) -> fst x"
    interpSuccessTest "\\(x : [nat, bool]) -> snd x"
    interpSuccessTest "\\(x : [nat, bool]) -> (snd x, fst x)"

  describe "Phase 2: Let Bindings with New Types" $ do
    tcTest "let p = (zero, True) in fst p" TNat
    interpTest "let p = (zero, True) in fst p" (V.VNat V.Zero)
    tcTest "let p = (zero, True) in snd p" TBool
    interpTest "let p = (zero, True) in snd p" (V.VBool True)
    tcTest "let u = tt in u" TTop
    interpTest "let u = tt in u" V.VTt

  describe "Phase 2: Function Definitions with New Types" $ do
    tcTest "fun swap (p : [nat, bool]) = (snd p, fst p); swap (zero, True)" (TPair TBool TNat)
    interpTest "fun swap (p : [nat, bool]) = (snd p, fst p); swap (zero, True)" (V.VPair (V.VBool True) (V.VNat V.Zero))
    
    tcTest "fun getUnit (x : nat) = tt; getUnit zero" TTop
    interpTest "fun getUnit (x : nat) = tt; getUnit zero" V.VTt

  describe "Phase 2: Error Cases" $ do
    -- Type errors
    tcErrorTest "fst zero"
    tcErrorTest "snd True"
    tcErrorTest "fst tt"
    tcErrorTest "snd tt"
    
    -- Evaluation errors
    interpErrorTest "fst zero"
    interpErrorTest "snd True"
    interpErrorTest "fst tt"
    interpErrorTest "snd tt"

  describe "Phase 2: Backward Compatibility" $ do
    -- Ensure Phase 1 features still work
    tcTest "U" TU
    interpTest "U" V.VU
    tcTest "nat" TU
    interpTest "nat" V.VU
    tcTest "bool" TU
    interpTest "bool" V.VU
    tcTest "zero" TNat
    interpTest "zero" (V.VNat V.Zero)
    tcTest "True" TBool
    interpTest "True" (V.VBool True)
    tcTest "\\(x : nat) -> x" (TDepFun (Ident "x") TNat TNat)
    interpSuccessTest "\\(x : nat) -> x" 