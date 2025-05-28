module TypeCheck.DBExpr where

import DBEnv
import DeBruijn
import Evaluator
import Lang.Abs (Type (..))
import Value (TClosure (TFun))

-- Arithmetic type checking helper
arithmetic :: (DBExp, DBExp) -> (DBEnv Type, FunEnv TClosure) -> Result Type
arithmetic (e1, e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  case (t1, t2) of
    (TNat, TNat) -> return TNat
    _ -> throw "Arithmetic can only be performed on natural numbers"

-- Logic type checking helper
logic :: (DBExp, DBExp) -> (DBEnv Type, FunEnv TClosure) -> Result Type
logic (e1, e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  case (t1, t2) of
    (TBool, TBool) -> return TBool
    _ -> throw "Boolean operations can only be performed on booleans"

-- Comparison type checking helper
comparison :: (DBExp, DBExp) -> (DBEnv Type, FunEnv TClosure) -> Result Type
comparison (e1, e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  case (t1, t2) of
    (TNat, TNat) -> return TBool
    (t1, t2) -> throw $ "Cannot compare " ++ show t1 ++ " with " ++ show t2

-- DE BRUIJN TYPE CHECKER -------------------------------------------------------------

infer :: DBExp -> (DBEnv Type, FunEnv TClosure) -> Result Type
-- Variables (De Bruijn indices)
infer (DBVar i) (types, _) =
  case lookupDB i types of
    Just t -> return t
    Nothing -> throw $ "Variable index " ++ show i ++ " out of bounds"
-- Natural numbers
infer DBZero _ = return TNat
infer (DBSuc e) env = do
  t <- infer e env
  case t of
    TNat -> return TNat
    _ -> throw "Successor can only be applied to natural numbers"

-- Arithmetic
infer (DBMul e1 e2) env = arithmetic (e1, e2) env
infer (DBAdd e1 e2) env = arithmetic (e1, e2) env
-- Booleans
infer DBTrue _ = return TBool
infer DBFalse _ = return TBool
infer (DBNot e) env = do
  t <- infer e env
  case t of
    TBool -> return TBool
    _ -> throw "Boolean operations can only be performed on booleans"
infer (DBAnd e1 e2) env = logic (e1, e2) env
infer (DBOr e1 e2) env = logic (e1, e2) env
-- Comparisons
infer (DBEq e1 e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  if t1 == t2
    then return TBool
    else throw "Cannot compare different types"
infer (DBLt e1 e2) env = comparison (e1, e2) env
infer (DBGt e1 e2) env = comparison (e1, e2) env
infer (DBLeq e1 e2) env = comparison (e1, e2) env
infer (DBGeq e1 e2) env = comparison (e1, e2) env
-- Control flow
infer (DBIf c iff els) env = do
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
infer (DBLet e body) env@(types, funs) = do
  t <- infer e env
  infer body (extendDB t types, funs)

-- Function application
infer (DBApp f e) env@(_, funs) = do
  case lookupFun f funs of
    Just (TFun targ tret) -> do
      eT <- infer e env
      if eT == targ
        then return tret
        else throw "Function argument type mismatch"
    Nothing -> throw $ "Function " ++ show f ++ " not found"