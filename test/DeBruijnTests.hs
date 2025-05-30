module DeBruijnTests where

import Control.Exception (evaluate)
import DBEnv
import DeBruijn
import Interp.DBExpr qualified as DBE
import Lang.Abs
import Test.Hspec
import TypeCheck.DBExpr qualified as DBTC
import Value qualified as V

-- Helper function to create VNat values easily
nat :: Integer -> V.Value
nat = V.VNat . V.toNat

-- Test conversion from named expressions to De Bruijn
conversionTest :: String -> Exp -> DBExp -> Spec
conversionTest desc namedExp expectedDB =
  it desc $ do
    toDB [] namedExp `shouldBe` expectedDB

-- Test De Bruijn expression evaluation
dbInterpTest :: String -> DBExp -> V.Value -> Spec
dbInterpTest desc dbExp expected =
  it desc $ do
    DBE.interp dbExp (emptyDB, emptyFun) `shouldBe` Right expected

-- Test De Bruijn type checking
dbTypeTest :: String -> DBExp -> Type -> Spec
dbTypeTest desc dbExp expectedType =
  it desc $ do
    DBTC.infer dbExp (emptyDB, emptyFun) `shouldBe` Right expectedType

-- Test De Bruijn bidirectional type checking
dbCheckTest :: String -> DBExp -> Type -> Spec
dbCheckTest desc dbExp expectedType =
  it desc $ do
    DBTC.check dbExp expectedType (emptyDB, emptyFun) `shouldBe` Right ()

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

    conversionTest
      "unbound variable converts to function reference"
      (EVar (Ident "x"))
      (DBFunRef (Ident "x"))

  describe "De Bruijn Expression Evaluation" $ do
    dbInterpTest
      "DBZero evaluates to VNat Zero"
      DBZero
      (V.VNat V.Zero)

    dbInterpTest
      "DBSuc DBZero evaluates to VNat (Suc Zero)"
      (DBSuc DBZero)
      (V.VNat (V.Suc V.Zero))

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
      (V.VBool True)

    dbInterpTest
      "DBFalse evaluates correctly"
      DBFalse
      (V.VBool False)

    dbInterpTest
      "DBLet with variable reference works"
      (DBLet (DBSuc (DBSuc DBZero)) (DBAdd (DBVar 0) (DBSuc DBZero)))
      (nat 3) -- let x = 2 in x + 1 = 3
    dbInterpTest
      "nested DBLet works correctly"
      (DBLet (DBSuc DBZero) (DBLet (DBSuc (DBSuc DBZero)) (DBAdd (DBVar 1) (DBVar 0))))
      (nat 3) -- let x = 1 in let y = 2 in x + y = 3

    -- Test curried function application step by step
    dbInterpTest
      "curried addition function works"
      (DBApp (DBLam (DBAdd (DBVar 0) (DBSuc DBZero))) (DBSuc (DBSuc DBZero)))
      (nat 3) -- (\x -> x + 1) 2 = 3

    -- Test lambda with boolean operations
    dbInterpTest
      "lambda with boolean logic works"
      (DBApp (DBLam (DBNot (DBVar 0))) DBTrue)
      (V.VBool False) -- (\x -> !x) True = False
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

  describe "Lambda Expression Tests" $ do
    -- Test lambda conversion to De Bruijn
    conversionTest
      "lambda expression converts correctly"
      (ELam (Ident "x") (EAdd (EVar (Ident "x")) (ESuc EZero)))
      (DBLam (DBAdd (DBVar 0) (DBSuc DBZero)))

    -- Test lambda evaluation through function application
    dbInterpTest
      "lambda function application works"
      (DBApp (DBLam (DBAdd (DBVar 0) (DBSuc DBZero))) (DBSuc (DBSuc DBZero)))
      (nat 3) -- (\x -> x + 1) 2 = 3

    -- Test nested lambda
    dbInterpTest
      "simple nested lambda application works"
      (DBApp (DBLam (DBApp (DBLam (DBVar 0)) (DBVar 0))) (DBSuc DBZero))
      (nat 1) -- (\x -> (\y -> y) x) 1 = 1

    -- Test bidirectional type checking of lambdas
    dbCheckTest
      "lambda can be checked against function type"
      (DBLam (DBAdd (DBVar 0) (DBSuc DBZero)))
      (TFun TNat TNat) -- \x -> x + 1 : nat -> nat
    dbCheckTest
      "nested lambda can be checked against nested function type"
      (DBLam (DBLam (DBAdd (DBVar 1) (DBVar 0))))
      (TFun TNat (TFun TNat TNat)) -- \x -> \y -> x + y : nat -> nat -> nat

    -- Test that lambda type checking works in let bindings
    dbInterpTest
      "lambda in let binding works"
      (DBLet (DBLam (DBMul (DBVar 0) (DBVar 0))) (DBApp (DBVar 0) (DBSuc (DBSuc (DBSuc DBZero)))))
      (nat 9) -- let square = \x -> x * x in square 3 = 9
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

    -- Test shift operations on lambdas
    shiftTest
      "shift in lambda increases cutoff for body"
      0
      1
      (DBLam (DBVar 0))
      (DBLam (DBVar 0))

    shiftTest
      "shift in lambda affects free variables in body"
      0
      1
      (DBLam (DBVar 1))
      (DBLam (DBVar 2))

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

    -- Test substitution operations on lambdas
    substTest
      "substitute in lambda body with proper shifting"
      0
      (DBSuc DBZero)
      (DBLam (DBVar 1))
      (DBLam (DBSuc DBZero))

    substTest
      "substitute bound variable in lambda (should not substitute)"
      0
      (DBSuc DBZero)
      (DBLam (DBVar 0))
      (DBLam (DBVar 0))