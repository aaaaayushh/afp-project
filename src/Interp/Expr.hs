module Interp.Expr where

import Env
import Evaluator
import Lang.Abs (Exp (..), Ident)
import Value

-- Helper functions for natural number arithmetic
addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Suc m) n = Suc (addNat m n)

mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat (Suc m) n = addNat n (mulNat m n)

-- Helper functions for natural number comparisons
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

arithmetic :: (Exp, Exp) -> (Env Value, Env Closure) -> (Nat -> Nat -> Nat) -> Result Value
arithmetic (e1, e2) env f = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VNat (f n1 n2)
    _ -> throw "Arithmetic can only be performed on natural numbers"

logic :: (Exp, Exp) -> (Env Value, Env Closure) -> (Bool -> Bool -> Bool) -> Result Value
logic (e1, e2) env f = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VBool b1, VBool b2) -> return $ VBool (f b1 b2)
    _ -> throw "Boolean operations can only be performed on booleans"

-- EXPRESSION INTERPRETER ------------------------------------------------------------

interp :: Exp -> (Env Value, Env Closure) -> Result Value
-- Natural numbers
interp EZero _ = return $ VNat Zero
interp (ESuc e) env = do
  v <- interp e env
  case v of
    VNat n -> return $ VNat (Suc n)
    _ -> throw "Successor can only be applied to natural numbers"

-- Arithmetic
interp (EMul e1 e2) env = arithmetic (e1, e2) env mulNat
interp (EAdd e1 e2) env = arithmetic (e1, e2) env addNat
-- Booleans
interp ETrue _ = return $ VBool True
interp EFalse _ = return $ VBool False
interp (ENot e) env = do
  v <- interp e env
  case v of
    VBool b -> return $ VBool (not b)
    _ -> throw "Boolean operations can only be performed on booleans"
interp (EAnd e1 e2) env = logic (e1, e2) env (&&)
interp (EOr e1 e2) env = logic (e1, e2) env (||)
-- Comparisons
interp (EEq e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VBool b1, VBool b2) -> return $ VBool (b1 == b2)
    (VNat n1, VNat n2) -> return $ VBool (eqNat n1 n2)
    _ -> throw "Cannot compare different types"
interp (ELt e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (ltNat n1 n2)
    _ -> throw "Cannot compare different types"
interp (EGt e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (gtNat n1 n2)
    _ -> throw "Cannot compare different types"
interp (ELeq e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (leqNat n1 n2)
    _ -> throw "Cannot compare different types"
interp (EGeq e1 e2) env = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VBool (geqNat n1 n2)
    _ -> throw "Cannot compare different types"

-- Control flow
interp (EIf c iff els) env = do
  cond <- interp c env
  case cond of
    VBool True -> interp iff env
    VBool False -> interp els env
    _ -> throw "Condition must be a boolean"

-- Let bindings
interp (ELet x e body) env@(vars, funs) = do
  arg <- interp e env
  interp body (bind x arg vars, funs)
interp (EVar x) (vars, _) =
  case find x vars of
    Just val -> return val
    Nothing -> throw $ "Variable " ++ show x ++ " is not bound"
-- Functions
interp (EApp f e) env@(vars, funs) = do
  case f of
    EVar fname ->
      case find fname funs of
        Just (DBFun body) -> do
          arg <- interp e env
          -- For now, we'll need to convert this to work with De Bruijn
          -- This is a temporary fix - we should use the De Bruijn interpreter
          error "Old interpreter should not be used with De Bruijn closures"
        Just (NamedFun fname') -> do
          arg <- interp e env
          error "Named function closures not yet implemented in old interpreter"
        Nothing -> throw $ "Function " ++ show fname ++ " not found"
    _ -> do
      -- For lambda expressions and complex function expressions
      fval <- interp f env
      case fval of
        VLam closure -> do
          arg <- interp e env
          error "Lambda evaluation not implemented in old interpreter"
        _ -> throw "Cannot apply non-function"
