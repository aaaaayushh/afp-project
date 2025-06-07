module TypeCheck.DBExpr where

import DBEnv
import DeBruijn
import Evaluator
import Lang.Abs (Ident (..), Type (..))
import Value qualified as V

-- Type checking environment
type TyEnv = (DBEnv Type, FunEnv V.TClosure)

-- Convert DBType to Type for compatibility
dbTypeToType :: DBType -> Type
dbTypeToType DBTNat = TNat
dbTypeToType DBTBool = TBool
dbTypeToType DBTU = TU
dbTypeToType DBTTop = TTop
dbTypeToType DBTBot = TBot
dbTypeToType (DBTPair a b) = TPair (dbTypeToType a) (dbTypeToType b)
dbTypeToType (DBTFun a b) = TFun (dbTypeToType a) (dbTypeToType b)
dbTypeToType (DBTDepFun a b) = TDepFun (Ident "x") (dbTypeToType a) (dbTypeToType b) -- Use dummy variable name
dbTypeToType (DBTVar _) = error "Cannot convert type variable to concrete type"

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
      V.TDepFun argType retType -> TDepFun (Ident "x") argType retType -- Use dummy variable name
    Nothing -> throw $ "Function " ++ show f ++ " not found"
-- Lambda expressions cannot be inferred without context
infer (DBLam _) _ = throw "Cannot infer type of lambda expression without annotation"
-- Type-annotated lambda expressions
infer (DBLamAnn domType body) env@(types, funs) = do
  let domTypeConc = dbTypeToType domType
  codType <- infer body (extendDB domTypeConc types, funs)
  return $ TDepFun (Ident "x") domTypeConc codType -- Use dummy variable name
  -- Universe type
infer DBU _ = return TU
-- Type expressions in the expression language
infer DBExprNat _ = return TU
infer DBExprBool _ = return TU
infer DBExprTop _ = return TU
infer DBExprBot _ = return TU
infer (DBExprPair a b) env = do
  check a TU env
  check b TU env
  return TU
infer (DBExprFun a b) env = do
  check a TU env
  check b TU env
  return TU
infer (DBExprDepFun a b) env = do
  check a TU env
  check b TU env
  return TU
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
    TDepFun _ targ tret -> do
      check e targ env
      return tret -- For now, don't do dependent substitution
    _ -> throw "Cannot apply non-function"

-- Phase 2: Built-in types and operations
-- Unit element
infer DBTt _ = return TTop
-- Pair constructor
infer (DBPair e1 e2) env = do
  t1 <- infer e1 env
  t2 <- infer e2 env
  return $ TPair t1 t2
-- First projection
infer (DBFst e) env = do
  t <- infer e env
  case t of
    TPair t1 _ -> return t1
    _ -> throw "fst can only be applied to pairs"
-- Second projection
infer (DBSnd e) env = do
  t <- infer e env
  case t of
    TPair _ t2 -> return t2
    _ -> throw "snd can only be applied to pairs"
-- Magic function has type (A : U) -> Bot -> A, but we simplify to Bot -> a for any a
infer (DBMagic e) env = do
  check e TBot env
  -- Magic can return any type, but since we can't know what type is expected,
  -- we'll need to handle this in the check function instead
  throw "Cannot infer type of magic function - must be checked against expected type"
-- Boolean eliminator: elimBool : (P : bool -> U) -> P true -> P false -> (b : bool) -> P b
-- For simplicity, we'll type it as: (bool -> U) -> A -> A -> bool -> A
infer (DBElimBool p t f b) env = do
  -- Check that p has type bool -> U
  check p (TFun TBool TU) env
  -- Check that b has type bool
  check b TBool env
  -- Infer the type of the true branch
  tType <- infer t env
  -- Check that the false branch has the same type
  check f tType env
  return tType

-- Bidirectional type checking
check :: DBExp -> Type -> TyEnv -> Result ()
-- Universe type checking - types have type U
check DBExprNat TU _ = return ()
check DBExprBool TU _ = return ()
check DBExprTop TU _ = return ()
check DBExprBot TU _ = return ()
check (DBExprPair a b) TU env = do
  check a TU env
  check b TU env
check DBU TU _ = return ()
check (DBExprFun a b) TU env = do
  check a TU env
  check b TU env
check (DBExprDepFun a b) TU env@(types, funs) = do
  check a TU env
  check b TU env

-- Lambda expressions are best checked against function types
check (DBLam body) (TFun targ tret) env@(types, funs) = do
  check body tret (extendDB targ types, funs)
check (DBLam body) (TDepFun _ targ tret) env@(types, funs) = do
  check body tret (extendDB targ types, funs)
check (DBLam _) t _ = throw $ "Lambda expression cannot have type " ++ show t
-- Type-annotated lambda
check (DBLamAnn domType body) (TFun targ tret) env@(types, funs) = do
  let domTypeConc = dbTypeToType domType
  if domTypeConc == targ
    then check body tret (extendDB domTypeConc types, funs)
    else throw "Lambda domain type mismatch"
check (DBLamAnn domType body) (TDepFun _ targ tret) env@(types, funs) = do
  let domTypeConc = dbTypeToType domType
  if domTypeConc == targ
    then check body tret (extendDB domTypeConc types, funs)
    else throw "Lambda domain type mismatch"

-- Phase 2: Magic function can be checked against any type as long as it's applied to Bot
check (DBMagic e) expectedType env = do
  check e TBot env
  return () -- Magic can have any type

-- For other expressions, infer and compare
check e expected env = do
  actual <- infer e env
  if actual == expected
    then return ()
    else throw $ "Type mismatch: expected " ++ show expected ++ " but got " ++ show actual

-- Helper function to convert types to expressions for type checking
typeToDBExp :: Type -> DBExp
typeToDBExp TNat = DBExprNat
typeToDBExp TBool = DBExprBool
typeToDBExp TU = DBU
typeToDBExp TTop = DBExprTop
typeToDBExp TBot = DBExprBot
typeToDBExp (TPair a b) = DBExprPair (typeToDBExp a) (typeToDBExp b)
typeToDBExp (TFun a b) = DBExprFun (typeToDBExp a) (typeToDBExp b)
typeToDBExp (TDepFun _ a b) = DBExprDepFun (typeToDBExp a) (typeToDBExp b)

-- Helper function to convert Type to DBType
typeToDBType :: Type -> DBType
typeToDBType TNat = DBTNat
typeToDBType TBool = DBTBool
typeToDBType TU = DBTU
typeToDBType TTop = DBTTop
typeToDBType TBot = DBTBot
typeToDBType (TPair a b) = DBTPair (typeToDBType a) (typeToDBType b)
typeToDBType (TFun a b) = DBTFun (typeToDBType a) (typeToDBType b)
typeToDBType (TDepFun _ a b) = DBTDepFun (typeToDBType a) (typeToDBType b)