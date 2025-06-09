module Phase3Tests where

import Data.Either (isLeft, isRight)
import Lang.Abs (Exp (..), Ident (..), Type (..))
import Run (infertype, run)
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

-- Helper function for successful evaluation tests (don't care about the result)
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
  describe "Phase 3: Vector Type Construction" $ do
    -- Vector type expressions have type U (they are types, so their type is the universe)
    tcTest "Vector (nat) (zero)" TU
    tcTest "Vector (bool) (suc zero)" TU
    tcTest "Vector (Top) (suc (suc zero))" TU

    -- Vector type expressions should evaluate to VType
    interpSuccessTest "Vector (nat) (zero)"

  describe "Phase 3: Empty Vector" $ do
    -- Empty vector needs type annotation since we can't infer the element type
    tcErrorTest "[]"

  describe "Phase 3: Vector Construction with Cons" $ do
    -- Basic cons operations
    interpTest "zero :: []" (V.VVec [V.VNat V.Zero])
    interpTest "True :: []" (V.VVec [V.VBool True])
    interpTest "tt :: []" (V.VVec [V.VTop])

    -- Multiple elements
    interpTest "zero :: (suc zero) :: []" (V.VVec [V.VNat V.Zero, V.VNat (V.Suc V.Zero)])
    interpTest "True :: False :: []" (V.VVec [V.VBool True, V.VBool False])

    -- Nested cons
    interpTest
      "zero :: (suc zero) :: (suc (suc zero)) :: []"
      (V.VVec [V.VNat V.Zero, V.VNat (V.Suc V.Zero), V.VNat (V.Suc (V.Suc V.Zero))])

  describe "Phase 3: Vector Head Operation" $ do
    -- Basic head operations
    interpTest "head (zero :: [])" (V.VNat V.Zero)
    interpTest "head (True :: [])" (V.VBool True)
    interpTest "head (tt :: [])" V.VTop

    -- Head of multi-element vectors
    interpTest "head (zero :: (suc zero) :: [])" (V.VNat V.Zero)
    interpTest "head (True :: False :: [])" (V.VBool True)

    -- Error cases
    interpErrorTest "head []"

  describe "Phase 3: Vector Tail Operation" $ do
    -- Basic tail operations
    interpTest "tail (zero :: [])" (V.VVec [])
    interpTest "tail (True :: [])" (V.VVec [])
    interpTest "tail (tt :: [])" (V.VVec [])

    -- Tail of multi-element vectors
    interpTest "tail (zero :: (suc zero) :: [])" (V.VVec [V.VNat (V.Suc V.Zero)])
    interpTest "tail (True :: False :: [])" (V.VVec [V.VBool False])
    interpTest
      "tail (zero :: (suc zero) :: (suc (suc zero)) :: [])"
      (V.VVec [V.VNat (V.Suc V.Zero), V.VNat (V.Suc (V.Suc V.Zero))])

    -- Error cases
    interpErrorTest "tail []"

  describe "Phase 3: Vector Append Operation" $ do
    -- Basic append operations (skip append [] [] as it requires type annotation)
    interpTest "append (zero :: []) []" (V.VVec [V.VNat V.Zero])
    interpTest "append [] (zero :: [])" (V.VVec [V.VNat V.Zero])
    interpTest
      "append (zero :: []) (suc zero :: [])"
      (V.VVec [V.VNat V.Zero, V.VNat (V.Suc V.Zero)])

    -- Multi-element append
    interpTest
      "append (zero :: (suc zero) :: []) (suc (suc zero) :: [])"
      (V.VVec [V.VNat V.Zero, V.VNat (V.Suc V.Zero), V.VNat (V.Suc (V.Suc V.Zero))])
    interpTest
      "append (True :: []) (False :: [])"
      (V.VVec [V.VBool True, V.VBool False])

  describe "Phase 3: Complex Vector Operations" $ do
    -- Combining operations
    interpTest "head (tail (zero :: (suc zero) :: []))" (V.VNat (V.Suc V.Zero))
    interpTest
      "tail (tail (zero :: (suc zero) :: (suc (suc zero)) :: []))"
      (V.VVec [V.VNat (V.Suc (V.Suc V.Zero))])
    interpTest "head (append (zero :: []) (suc zero :: []))" (V.VNat V.Zero)
    interpTest "tail (append (zero :: []) (suc zero :: []))" (V.VVec [V.VNat (V.Suc V.Zero)])

  describe "Phase 3: Vector Functions" $ do
    -- Functions that work with vectors - using correct Vector syntax
    interpSuccessTest "\\(v : Vector (nat) (zero)) -> v"
    interpSuccessTest "\\(v : Vector (nat) (suc zero)) -> head v"
    interpSuccessTest "\\(v : Vector (nat) (suc zero)) -> tail v"
    interpSuccessTest "\\(v1 : Vector (nat) (zero)) -> \\(v2 : Vector (nat) (zero)) -> append v1 v2"

  describe "Phase 3: Error Cases" $ do
    -- Type errors
    tcErrorTest "head zero"
    tcErrorTest "tail True"
    tcErrorTest "append zero []"
    tcErrorTest "append [] True"

    -- Runtime errors
    interpErrorTest "head []"
    interpErrorTest "tail []"