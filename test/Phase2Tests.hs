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
  describe "Phase 2: Top Type" $ do
    -- Top type as a type
    tcTest "Top" TU
    interpTest "Top" (V.VType TTop)
    
    -- tt value
    tcTest "tt" TTop
    interpTest "tt" V.VTop
    
    -- Functions involving Top
    tcTest "\\(x : Top) -> x" (TDepFun (Ident "x") TTop TTop)
    interpSuccessTest "\\(x : Top) -> x"
    interpSuccessTest "(\\(x : Top) -> x) tt"

  describe "Phase 2: Bot Type" $ do
    -- Bot type as a type
    tcTest "Bot" TU
    interpTest "Bot" (V.VType TBot)
    
    -- magic function (simplified typing)
    tcTest "magic" (TFun TBot TBot)
    interpSuccessTest "magic"

  describe "Phase 2: Pair Types" $ do
    -- Pair type construction
    tcTest "[nat, bool]" TU
    interpTest "[nat, bool]" (V.VType (TPair TNat TBool))
    tcTest "[Top, Bot]" TU
    interpTest "[Top, Bot]" (V.VType (TPair TTop TBot))
    
    -- Pair values
    tcTest "(zero, True)" (TPair TNat TBool)
    interpTest "(zero, True)" (V.VPair (V.VNat V.Zero) (V.VBool True))
    
    tcTest "(tt, False)" (TPair TTop TBool)
    interpTest "(tt, False)" (V.VPair V.VTop (V.VBool False))
    
    -- Nested pairs
    tcTest "((zero, True), (suc zero, False))" (TPair (TPair TNat TBool) (TPair TNat TBool))
    interpSuccessTest "((zero, True), (suc zero, False))"

  describe "Phase 2: Pair Projections" $ do
    -- First projection
    tcTest "fst((zero, True))" TNat
    interpTest "fst((zero, True))" (V.VNat V.Zero)
    
    tcTest "fst((tt, False))" TTop
    interpTest "fst((tt, False))" V.VTop
    
    -- Second projection
    tcTest "snd((zero, True))" TBool
    interpTest "snd((zero, True))" (V.VBool True)
    
    tcTest "snd((tt, False))" TBool
    interpTest "snd((tt, False))" (V.VBool False)
    
    -- Complex projections
    tcTest "fst(fst(((zero, True), (suc zero, False))))" TNat
    interpTest "fst(fst(((zero, True), (suc zero, False))))" (V.VNat V.Zero)
    
    tcTest "snd(snd(((zero, True), (suc zero, False))))" TBool
    interpTest "snd(snd(((zero, True), (suc zero, False))))" (V.VBool False)

  describe "Phase 2: Pair Functions" $ do
    -- Functions returning pairs
    tcTest "\\(x : nat) -> (x, True)" (TDepFun (Ident "x") TNat (TPair TNat TBool))
    interpSuccessTest "\\(x : nat) -> (x, True)"
    interpTest "(\\(x : nat) -> (x, True)) zero" (V.VPair (V.VNat V.Zero) (V.VBool True))
    
    -- Functions taking pairs
    tcTest "\\(p : [nat, bool]) -> fst(p)" (TDepFun (Ident "x") (TPair TNat TBool) TNat)
    interpSuccessTest "\\(p : [nat, bool]) -> fst(p)"
    interpTest "(\\(p : [nat, bool]) -> fst(p)) (suc zero, False)" (V.VNat (V.Suc V.Zero))
    
    -- Swap function
    tcTest "\\(p : [nat, bool]) -> (snd(p), fst(p))" (TDepFun (Ident "x") (TPair TNat TBool) (TPair TBool TNat))
    interpSuccessTest "\\(p : [nat, bool]) -> (snd(p), fst(p))"
    interpTest "(\\(p : [nat, bool]) -> (snd(p), fst(p))) (zero, True)" (V.VPair (V.VBool True) (V.VNat V.Zero))

  describe "Phase 2: Boolean Eliminator - Basic" $ do
    -- Simple elimBool with same type branches
    tcTest "elimBool(\\(b : bool) -> nat)(zero)(suc zero)(True)" TNat
    interpTest "elimBool(\\(b : bool) -> nat)(zero)(suc zero)(True)" (V.VNat V.Zero)
    
    tcTest "elimBool(\\(b : bool) -> nat)(zero)(suc zero)(False)" TNat
    interpTest "elimBool(\\(b : bool) -> nat)(zero)(suc zero)(False)" (V.VNat (V.Suc V.Zero))
    
    -- elimBool with bool branches
    tcTest "elimBool(\\(b : bool) -> bool)(True)(False)(True)" TBool
    interpTest "elimBool(\\(b : bool) -> bool)(True)(False)(True)" (V.VBool True)
    
    tcTest "elimBool(\\(b : bool) -> bool)(True)(False)(False)" TBool
    interpTest "elimBool(\\(b : bool) -> bool)(True)(False)(False)" (V.VBool False)

  describe "Phase 2: Boolean Eliminator - Complex" $ do
    -- elimBool with Top/Bot
    tcTest "elimBool(\\(b : bool) -> Top)(tt)(tt)(True)" TTop
    interpTest "elimBool(\\(b : bool) -> Top)(tt)(tt)(True)" V.VTop
    
    -- elimBool with pairs
    tcTest "elimBool(\\(b : bool) -> [nat, bool])((zero, True))((suc zero, False))(True)" (TPair TNat TBool)
    interpTest "elimBool(\\(b : bool) -> [nat, bool])((zero, True))((suc zero, False))(True)" (V.VPair (V.VNat V.Zero) (V.VBool True))
    
    tcTest "elimBool(\\(b : bool) -> [nat, bool])((zero, True))((suc zero, False))(False)" (TPair TNat TBool)
    interpTest "elimBool(\\(b : bool) -> [nat, bool])((zero, True))((suc zero, False))(False)" (V.VPair (V.VNat (V.Suc V.Zero)) (V.VBool False))

  describe "Phase 2: Boolean Eliminator - Dependent Types (natOrBool)" $ do
    let p = "\\(b : bool) -> if b then nat else bool"
    let t = "suc(suc(suc(suc zero)))"
    let e = "False"
    
    -- Test when the boolean is True
    tcTest ("elimBool(" ++ p ++ ")(" ++ t ++ ")(" ++ e ++ ")(True)") TNat
    interpTest ("elimBool(" ++ p ++ ")(" ++ t ++ ")(" ++ e ++ ")(True)") (V.vnat 4)
    
    -- Test when the boolean is False
    tcTest ("elimBool(" ++ p ++ ")(" ++ t ++ ")(" ++ e ++ ")(False)") TBool
    interpTest ("elimBool(" ++ p ++ ")(" ++ t ++ ")(" ++ e ++ ")(False)") (V.VBool False)

  describe "Phase 2: Backward Compatibility" $ do
    -- Ensure Phase 1 features still work with Phase 2 extensions
    tcTest "suc zero" TNat
    tcTest "True && False" TBool
    tcTest "\\(x : nat) -> x + zero" (TDepFun (Ident "x") TNat TNat)
    
    interpTest "suc (suc zero)" (V.VNat (V.Suc (V.Suc V.Zero)))
    interpTest "True && False" (V.VBool False)
    interpSuccessTest "\\(x : nat) -> x + zero"
    
    -- Mixed Phase 1 and Phase 2
    tcTest "(suc zero, True && False)" (TPair TNat TBool)
    interpTest "(suc zero, True && False)" (V.VPair (V.VNat (V.Suc V.Zero)) (V.VBool False))

  describe "Phase 2: Error Cases" $ do
    -- Type errors with projections
    tcErrorTest "fst(zero)"
    tcErrorTest "snd(True)"
    tcErrorTest "fst(\\(x : nat) -> x)"
    
    -- Type errors with elimBool
    tcErrorTest "elimBool(zero)(True)(False)(True)"
    tcErrorTest "elimBool(\\(b : bool) -> nat)(True)(False)(zero)"
    
    -- Evaluation errors
    interpErrorTest "fst(zero)"
    interpErrorTest "snd(True)"

  describe "Phase 2: Advanced Combinations" $ do
    -- Combining all Phase 2 features
    tcTest "fst(elimBool(\\(b : bool) -> [Top, nat])((tt, zero))((tt, suc zero))(True))" TTop
    interpTest "fst(elimBool(\\(b : bool) -> [Top, nat])((tt, zero))((tt, suc zero))(True))" V.VTop
    
    tcTest "snd(elimBool(\\(b : bool) -> [Top, nat])((tt, zero))((tt, suc zero))(False))" TNat
    interpTest "snd(elimBool(\\(b : bool) -> [Top, nat])((tt, zero))((tt, suc zero))(False))" (V.VNat (V.Suc V.Zero))
    
    -- Complex nested structures
    tcTest "elimBool(\\(b : bool) -> [[nat, bool], Top])(((zero, True), tt))(((suc zero, False), tt))(True)" (TPair (TPair TNat TBool) TTop)
    interpSuccessTest "elimBool(\\(b : bool) -> [[nat, bool], Top])(((zero, True), tt))(((suc zero, False), tt))(True)" 