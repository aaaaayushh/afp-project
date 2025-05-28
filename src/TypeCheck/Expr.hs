module TypeCheck.Expr where

import Env
import Evaluator
import Lang.Abs
  ( Exp (..),
    Ident,
    Type (..),
  )
import Value (TClosure (TFun))

arithmetic :: (Exp, Exp) -> (Env Type, Env TClosure) -> Result Type
arithmetic (e1, e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  case (t1, t2) of
    (TNat, TNat) -> return TNat
    _ -> throw "Arithmetic can only be performed on natural numbers"

logic :: (Exp, Exp) -> (Env Type, Env TClosure) -> Result Type
logic (e1, e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  case (t1, t2) of
    (TBool, TBool) -> return TBool
    _ -> throw "Boolean operations can only be performed on booleans"

comparison :: (Exp, Exp) -> (Env Type, Env TClosure) -> Result Type
comparison (e1, e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  case (t1, t2) of
    (TNat, TNat) -> return TBool
    (t1, t2) -> throw $ "Cannot compare " ++ show t1 ++ " with " ++ show t2

-- EXPRESSION TYPE CHECKER -----------------------------------------------------------

infer :: Exp -> (Env Type, Env TClosure) -> Result Type
-- Natural numbers
infer EZero _ = return TNat
infer (ESuc e) env = do
  t <- infer e env
  case t of
    TNat -> return TNat
    _ -> throw "Successor can only be applied to natural numbers"

-- Arithmetic
infer (EMul e1 e2) env = arithmetic (e1, e2) env
infer (EAdd e1 e2) env = arithmetic (e1, e2) env
-- Booleans
infer ETrue _ = return TBool
infer EFalse _ = return TBool
infer (ENot e) env = do
  t <- infer e env
  case t of
    TBool -> return TBool
    _ -> throw "Boolean operations can only be performed on booleans"
infer (EAnd e1 e2) env = logic (e1, e2) env
infer (EOr e1 e2) env = logic (e1, e2) env
-- Comparisons
infer (EEq e1 e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  if t1 == t2
    then return TBool
    else throw "Cannot compare different types"
infer (ELt e1 e2) env = comparison (e1, e2) env
infer (EGt e1 e2) env = comparison (e1, e2) env
infer (ELeq e1 e2) env = comparison (e1, e2) env
infer (EGeq e1 e2) env = comparison (e1, e2) env
-- Control flow
infer (EIf c iff els) env = do
  cond <- infer c env
  case cond of
    TBool -> do
      tI <- infer iff env
      tE <- infer els env
      case (tI, tE) of
        (tI, tE) | tI == tE -> return tI
        _ -> throw "Both branches of an if must have the same type"
    _ -> throw "Condition must be a boolean"

-- Let bindings
infer (ELet x e body) env@(vars, funs) = do
  t <- infer e env
  infer body (bind x t vars, funs)
infer (EVar x) (vars, _) =
  case find x vars of
    Just t -> return t
    Nothing -> throw $ "Variable " ++ show x ++ " is not bound"
-- Functions
infer (EApp f e) env@(_, funs) = do
  case find f funs of
    Just (TFun targ tret) -> do
      eT <- infer e env
      if eT == targ
        then return tret
        else throw "Function argument type mismatch"
    _ -> throw "Cannot call non-function"
