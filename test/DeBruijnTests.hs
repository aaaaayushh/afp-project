module DeBruijnTests where

import Control.Exception (evaluate)
import DBEnv
import DeBruijn
import qualified Interp.DBExpr as DBE
import Lang.Abs
import Test.Hspec
import qualified TypeCheck.DBExpr as DBTC
import Value

-- Helper function to create VNat values easily
nat :: Integer -> Value
nat = VNat . toNat

-- Test conversion from named expressions to De Bruijn
conversionTest :: String -> Exp -> DBExp -> Spec
conversionTest desc namedExp expectedDB =
  it desc $ do
    toDB [] namedExp `shouldBe` expectedDB

-- Test De Bruijn expression evaluation
dbInterpTest :: String -> DBExp -> Value -> Spec
dbInterpTest desc dbExp expected =
  it desc $ do
    DBE.interp dbExp (emptyDB, emptyFun) `shouldBe` Right expected

-- Test De Bruijn type checking
dbTypeTest :: String -> DBExp -> Type -> Spec
dbTypeTest desc dbExp expectedType =
  it desc $ do
    DBTC.infer dbExp (emptyDB, emptyFun) `shouldBe` Right expectedType

-- Test shift operation
shiftTest :: String -> Int -> Int -> DBExp -> DBExp -> Spec
shiftTest desc n k input expected =
  it desc $ do
    shift n k input `shouldBe` expected

-- Test substitution operation
substTest :: String -> Int -> DBExp -> DBExp -> DBExp -> Spec
substTest desc n replacement input expected =
  it desc $ do
    subst n replacement input `shouldBe` expected

test :: IO ()
test = hspec $ do
  describe "De Bruijn Conversion Tests" $ do
    conversionTest
      "zero converts to DBZero"
      EZero
      DBZero

    conversionTest
      "suc zero converts correctly"
      (ESuc EZero)
      (DBSuc DBZero)

    conversionTest
      "addition converts correctly"
      (EAdd EZero (ESuc EZero))
      (DBAdd DBZero (DBSuc DBZero))

    conversionTest
      "let expression converts with correct indices"
      (ELet (Ident "x") EZero (EVar (Ident "x")))
      (DBLet DBZero (DBVar 0))

    it "variable with empty context should throw error" $ do
      evaluate (toDB [] (EVar (Ident "x"))) `shouldThrow` anyException

  describe "De Bruijn Expression Evaluation" $ do
    dbInterpTest
      "DBZero evaluates to VNat Zero"
      DBZero
      (VNat Zero)

    dbInterpTest
      "DBSuc DBZero evaluates to VNat (Suc Zero)"
      (DBSuc DBZero)
      (VNat (Suc Zero))

    dbInterpTest
      "DBAdd works correctly"
      (DBAdd (DBSuc DBZero) (DBSuc (DBSuc DBZero)))
      (nat 3)

    dbInterpTest
      "DBMul works correctly"
      (DBMul (DBSuc (DBSuc DBZero)) (DBSuc (DBSuc (DBSuc DBZero))))
      (nat 6)

    dbInterpTest
      "DBTrue evaluates correctly"
      DBTrue
      (VBool True)

    dbInterpTest
      "DBFalse evaluates correctly"
      DBFalse
      (VBool False)

    dbInterpTest
      "DBLet with variable reference works"
      (DBLet (DBSuc (DBSuc DBZero)) (DBAdd (DBVar 0) (DBSuc DBZero)))
      (nat 3) -- let x = 2 in x + 1 = 3
    dbInterpTest
      "nested DBLet works correctly"
      (DBLet (DBSuc DBZero) (DBLet (DBSuc (DBSuc DBZero)) (DBAdd (DBVar 1) (DBVar 0))))
      (nat 3) -- let x = 1 in let y = 2 in x + y = 3
  describe "De Bruijn Type Checking" $ do
    dbTypeTest
      "DBZero has type TNat"
      DBZero
      TNat

    dbTypeTest
      "DBTrue has type TBool"
      DBTrue
      TBool

    dbTypeTest
      "DBAdd has type TNat"
      (DBAdd DBZero (DBSuc DBZero))
      TNat

    dbTypeTest
      "DBLt has type TBool"
      (DBLt DBZero (DBSuc DBZero))
      TBool

    dbTypeTest
      "DBLet preserves inner type"
      (DBLet DBZero (DBVar 0))
      TNat

  describe "De Bruijn Shift Operation" $ do
    shiftTest
      "shift variable below cutoff unchanged"
      2
      1
      (DBVar 0)
      (DBVar 0)

    shiftTest
      "shift variable at cutoff"
      1
      2
      (DBVar 1)
      (DBVar 3)

    shiftTest
      "shift variable above cutoff"
      1
      2
      (DBVar 2)
      (DBVar 4)

    shiftTest
      "shift in DBLet increases cutoff"
      0
      1
      (DBLet DBZero (DBVar 0))
      (DBLet DBZero (DBVar 0))

    shiftTest
      "shift in DBLet affects outer variables"
      0
      1
      (DBLet DBZero (DBVar 1))
      (DBLet DBZero (DBVar 2))

  describe "De Bruijn Substitution Operation" $ do
    substTest
      "substitute exact match"
      0
      (DBSuc DBZero)
      (DBVar 0)
      (DBSuc DBZero)

    substTest
      "substitute and decrement higher indices"
      0
      (DBSuc DBZero)
      (DBVar 1)
      (DBVar 0)

    substTest
      "substitute in DBLet body"
      0
      (DBSuc DBZero)
      (DBLet DBZero (DBVar 1))
      (DBLet DBZero (DBSuc DBZero))

    substTest
      "substitute with shift in DBLet"
      0
      (DBVar 0)
      (DBLet DBZero (DBVar 1))
      (DBLet DBZero (DBVar 1))