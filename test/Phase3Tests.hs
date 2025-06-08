module Phase3Tests where

import Run (run)
import Test.HUnit
import Value (Value)

-- Helper to format the result for comparison
formatResult :: Either String Value -> Either String String
formatResult (Right val) = Right (show val)
formatResult (Left err) = Left err

-- Test cases for the Identity type (Phase 3)

-- Test for refl's type
test_refl_type :: Test
test_refl_type =
  TestCase $
    assertEqual
      "refl type"
      (Right "Id nat zero zero")
      (formatResult $ run "refl : Id nat zero zero")

-- Symmetry of equality
test_sym :: Test
test_sym =
  TestCase $
    assertEqual
      "sym"
      (Right "VRefl")
      (formatResult $ run "let sym = (\\a x y (eq : Id a x y) -> J a x (\\y' eq' -> Id a y' x) (refl a x) y eq) in sym nat zero zero refl")

-- Transitivity of equality
test_trans :: Test
test_trans =
  TestCase $
    assertEqual
      "trans"
      (Right "VRefl")
      (formatResult $ run "let trans = (\\a x y z (p : Id a x y) (q : Id a y z) -> J a y (\\y' eq -> Id a x y') p z q) in trans nat zero zero zero refl refl")

-- Congruence
test_cong :: Test
test_cong =
  TestCase $
    assertEqual
      "cong"
      (Right "VRefl")
      (formatResult $ run "let cong = (\\a b (f : a -> b) x y (eq : Id a x y) -> J a x (\\y' eq' -> Id b (f x) (f y')) (refl b (f x)) y eq) in cong nat nat (\\n -> n) zero zero refl")

-- Substitution
test_subst :: Test
test_subst =
  TestCase $
    assertEqual
      "subst"
      (Right "VRefl")
      (formatResult $ run "let subst = (\\a (p : a -> U) x y (eq : Id a x y) (px : p x) -> J a x (\\y' eq' -> p y') px y eq) in subst nat (\\n -> Id nat n n) zero zero refl refl")

phase3Tests :: Test
phase3Tests =
  TestList
    [ "refl_type" ~: test_refl_type,
      "sym" ~: test_sym,
      "trans" ~: test_trans,
      "cong" ~: test_cong,
      "subst" ~: test_subst
    ]