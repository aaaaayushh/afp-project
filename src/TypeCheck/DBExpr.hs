module TypeCheck.DBExpr where

import DBEnv
import DeBruijn (DBExp (..), DBType (..), dbTypeToType)
import Evaluator
import Lang.Abs (Type (..))
import Value qualified as V

-- Type checking environment
type TyEnv = (DBEnv Type, FunEnv V.TClosure)

-- Check that a type is well-formed (all components have correct types)
checkTypeWellFormed :: DBType -> TyEnv -> Result ()
checkTypeWellFormed DBTNat _ = return ()
checkTypeWellFormed DBTBool _ = return ()
checkTypeWellFormed DBTUniverse _ = return ()
checkTypeWellFormed (DBTFun t1 t2) env = do
  checkTypeWellFormed t1 env
  checkTypeWellFormed t2 env
checkTypeWellFormed (DBTDepFun t1 t2) env@(types, funs) = do
  checkTypeWellFormed t1 env
  -- For dependent functions, check the codomain in extended context
  -- We need a dummy value for the context, but since we're only checking well-formedness,
  -- we can use a placeholder
  checkTypeWellFormed t2 (extendDB TUniverse types, funs) -- Placeholder type
checkTypeWellFormed (DBTVar _) _ = return () -- Type variables are assumed well-formed

-- Bidirectional type inference
infer :: DBExp -> TyEnv -> Result Type
-- Variables (De Bruijn indices)
infer (DBVar i) (types, _) =
  case lookupDB i types of
    Just t -> return t
    Nothing -> throw $ "Variable index " ++ show i ++ " out of bounds"
-- Function references
infer (DBFunRef f) (_, funs) =
  case lookupFun f funs of
    Just tclosure -> return $ case tclosure of
      V.TFun argType retType -> TFun argType retType
    Nothing -> throw $ "Function " ++ show f ++ " not found"
-- Types as expressions have type U (universe)
infer (DBType t) env = do
  checkTypeWellFormed t env
  return TUniverse
-- Lambda expressions cannot be inferred without context
infer (DBLam _) _ = throw "Cannot infer type of lambda expression without annotation"
-- Typed lambda expressions can be inferred
infer (DBTypedLam argType body) env@(types, funs) = do
  let argTypeConverted = dbTypeToType argType
  retType <- infer body (extendDB argTypeConverted types, funs)
  return $ TFun argTypeConverted retType
-- Natural numbers
infer DBZero _ = return TNat
infer (DBSuc e) env = do
  t <- infer e env
  case t of
    TNat -> return TNat
    _ -> throw "Successor can only be applied to natural numbers"

-- Arithmetic
infer (DBAdd e1 e2) env = do
  check e1 TNat env
  check e2 TNat env
  return TNat
infer (DBMul e1 e2) env = do
  check e1 TNat env
  check e2 TNat env
  return TNat

-- Booleans
infer DBTrue _ = return TBool
infer DBFalse _ = return TBool
infer (DBNot e) env = do
  check e TBool env
  return TBool
infer (DBAnd e1 e2) env = do
  check e1 TBool env
  check e2 TBool env
  return TBool
infer (DBOr e1 e2) env = do
  check e1 TBool env
  check e2 TBool env
  return TBool

-- Comparisons
infer (DBEq e1 e2) env = do
  t1 <- infer e1 env
  check e2 t1 env
  return TBool
infer (DBLt e1 e2) env = do
  check e1 TNat env
  check e2 TNat env
  return TBool
infer (DBGt e1 e2) env = do
  check e1 TNat env
  check e2 TNat env
  return TBool
infer (DBLeq e1 e2) env = do
  check e1 TNat env
  check e2 TNat env
  return TBool
infer (DBGeq e1 e2) env = do
  check e1 TNat env
  check e2 TNat env
  return TBool

-- Control flow
infer (DBIf c t e) env = do
  check c TBool env
  tT <- infer t env
  check e tT env
  return tT

-- Let bindings
infer (DBLet e body) env@(types, funs) = do
  t <- infer e env
  infer body (extendDB t types, funs)

-- Function application
infer (DBApp f e) env = do
  fType <- infer f env
  case fType of
    TFun targ tret -> do
      check e targ env
      return tret
    _ -> throw "Cannot apply non-function"

-- Bidirectional type checking
check :: DBExp -> Type -> TyEnv -> Result ()
-- Lambda expressions are best checked against function types
check (DBLam body) (TFun targ tret) env@(types, funs) = do
  check body tret (extendDB targ types, funs)
check (DBLam _) t _ = throw $ "Lambda expression cannot have type " ++ show t
-- Typed lambda expressions
check (DBTypedLam argType body) (TFun targ tret) env@(types, funs) = do
  let argTypeConverted = dbTypeToType argType
  if argTypeConverted == targ
    then check body tret (extendDB argTypeConverted types, funs)
    else throw $ "Type mismatch: lambda expects " ++ show argTypeConverted ++ " but got " ++ show targ
check (DBTypedLam _ _) t _ = throw $ "Typed lambda expression cannot have type " ++ show t
-- For other expressions, infer and compare
check e expected env = do
  actual <- infer e env
  if actual == expected
    then return ()
    else throw $ "Type mismatch: expected " ++ show expected ++ " but got " ++ show actual