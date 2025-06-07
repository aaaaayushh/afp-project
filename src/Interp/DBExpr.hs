module Interp.DBExpr where

import DBEnv
import DeBruijn
import Evaluator
import Lang.Abs (Ident (..), Type (..))
import Value (Closure (..), Nat (..), Value (..))
import Value qualified

-- Helper functions for natural number arithmetic (same as before)
addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Suc m) n = Suc (addNat m n)

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat (Suc m) n = addNat n (mulNat m n)

-- Helper functions for natural number comparisons (same as before)
ltNat :: Nat -> Nat -> Bool
ltNat Zero (Suc _) = True
ltNat (Suc m) (Suc n) = ltNat m n
ltNat _ _ = False

leqNat :: Nat -> Nat -> Bool
leqNat Zero _ = True
leqNat (Suc m) (Suc n) = leqNat m n
leqNat _ _ = False

gtNat :: Nat -> Nat -> Bool
gtNat m n = ltNat n m

geqNat :: Nat -> Nat -> Bool
geqNat m n = leqNat n m

eqNat :: Nat -> Nat -> Bool
eqNat Zero Zero = True
eqNat (Suc m) (Suc n) = eqNat m n
eqNat _ _ = False

-- Arithmetic helper for De Bruijn expressions
arithmetic :: (DBExp, DBExp) -> (DBEnv Value, FunEnv Closure) -> (Nat -> Nat -> Nat) -> Result Value
arithmetic (e1, e2) env f = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VNat (f n1 n2)
    _ -> throw "Arithmetic can only be performed on natural numbers"

-- Logic helper for De Bruijn expressions
logic :: (DBExp, DBExp) -> (DBEnv Value, FunEnv Closure) -> (Bool -> Bool -> Bool) -> Result Value
logic (e1, e2) env f = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VBool b1, VBool b2) -> return $ VBool (f b1 b2)
    _ -> throw "Boolean operations can only be performed on booleans"

-- DE BRUIJN EXPRESSION INTERPRETER --------------------------------------------------

interp :: DBExp -> (DBEnv Value, FunEnv Closure) -> Result Value
-- Variables (De Bruijn indices)
interp (DBVar i) (vals, _) =
  case lookupDB i vals of
    Just val -> return val
    Nothing -> throw $ "Variable index " ++ show i ++ " out of bounds"
-- Function references
interp (DBFunRef f) (_, funs) =
  case lookupFun f funs of
    Just closure -> return $ VLam closure
    Nothing -> throw $ "Function " ++ show f ++ " not found"
-- Universe type
interp DBU _ = return VU
-- Type expressions evaluate to universe (for Church encoding)
interp DBExprNat _ = return (VType TNat)
interp DBExprBool _ = return (VType TBool)
interp (DBExprFun a b) env = do
  va <- interp a env
  vb <- interp b env
  case (va, vb) of
    (VType ta, VType tb) -> return (VType (TFun ta tb))
    (VType ta, VU) -> return (VType (TFun ta TU))
    (VU, VType tb) -> return (VType (TFun TU tb))
    (VU, VU) -> return (VType (TFun TU TU))
    _ -> throw "Arguments to function type constructor '->' must be types"
interp (DBExprDepFun a b) env = throw "Dependent function types cannot be fully evaluated at runtime in this version"
-- Phase 2: Top/Bot type expressions
interp DBExprTop _ = return (VType TTop)
interp DBExprBot _ = return (VType TBot)
-- Phase 2: Pair type expressions
interp (DBExprPair a b) env = do
  va <- interp a env
  vb <- interp b env
  case (va, vb) of
    (VType ta, VType tb) -> return (VType (TPair ta tb))
    _ -> throw "Arguments to pair type constructor '[,]' must be types"
-- Natural numbers
interp DBZero _ = return $ VNat Zero
interp (DBSuc e) env = do
  v <- interp e env
  case v of
    VNat n -> return $ VNat (Suc n)
    _ -> throw "Successor can only be applied to natural numbers"

-- Arithmetic
interp (DBMul e1 e2) env = arithmetic (e1, e2) env mulNat
interp (DBAdd e1 e2) env = arithmetic (e1, e2) env addNat
-- Booleans
interp DBTrue _ = return $ VBool True
interp DBFalse _ = return $ VBool False
interp (DBNot e) env = do
  v <- interp e env
  case v of
    VBool b -> return $ VBool (not b)
    _ -> throw "Boolean operations can only be performed on booleans"
interp (DBAnd e1 e2) env = logic (e1, e2) env (&&)
interp (DBOr e1 e2) env = logic (e1, e2) env (||)
-- Comparisons
interp (DBEq e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VBool b1, VBool b2) -> return $ VBool (b1 == b2)
    (VNat n1, VNat n2) -> return $ VBool (eqNat n1 n2)
    (VType t1, VType t2) -> return $ VBool (t1 == t2)
    _ -> throw "Cannot compare different types"
interp (DBLt e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (ltNat n1 n2)
    _ -> throw "Cannot compare different types"
interp (DBGt e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (gtNat n1 n2)
    _ -> throw "Cannot compare different types"
interp (DBLeq e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (leqNat n1 n2)
    _ -> throw "Cannot compare different types"
interp (DBGeq e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (geqNat n1 n2)
    _ -> throw "Cannot compare different types"

-- Control flow
interp (DBIf c iff els) env = do
  cond <- interp c env
  case cond of
    VBool True -> interp iff env
    VBool False -> interp els env
    _ -> throw "Condition must be a boolean"

-- Let bindings
interp (DBLet e body) env@(vals, funs) = do
  val <- interp e env
  interp body (extendDB val vals, funs)

-- Lambda expressions
interp (DBLam body) env = return $ VLam (DBFun body)
-- Type-annotated lambda expressions (ignore type annotation at runtime)
interp (DBLamAnn _ body) env = return $ VLam (DBFun body)
-- Function application
interp (DBApp f e) env@(vals, funs) = do
  fval <- interp f env
  arg <- interp e env
  case fval of
    VLam (DBFun body) -> do
      -- Apply lambda: substitute argument for De Bruijn index 0
      interp body (extendDB arg vals, funs)
    VLam (NamedFun fname) -> do
      -- Look up named function and apply
      case lookupFun fname funs of
        Just (DBFun body) ->
          interp body (extendDB arg vals, funs)
        Nothing -> throw $ "Named function " ++ show fname ++ " not found"
    _ -> throw "Cannot apply non-function"

-- Phase 2: Top/Bot values
interp DBTt _ = return VTop
interp DBMagic _ = return $ VLam (DBFun (DBVar 0)) -- magic as identity (impossible to call properly)

-- Phase 2: Boolean eliminator
interp (DBElimBool p t f b) env = do
  bval <- interp b env
  case bval of
    VBool True -> interp t env
    VBool False -> interp f env
    _ -> throw "elimBool requires boolean argument"

-- Phase 2: Pair types
interp (DBPair a b) env = do
  va <- interp a env
  vb <- interp b env
  return $ VPair va vb
interp (DBFst p) env = do
  vp <- interp p env
  case vp of
    VPair va _ -> return va
    _ -> throw "fst requires pair argument"
interp (DBSnd p) env = do
  vp <- interp p env
  case vp of
    VPair _ vb -> return vb
    _ -> throw "snd requires pair argument"
