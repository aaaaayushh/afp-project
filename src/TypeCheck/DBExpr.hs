module TypeCheck.DBExpr where

import DBEnv
import DeBruijn
import Evaluator
import Interp.DBExpr (interp)
import Lang.Abs (Exp (..), Ident (..), Type (..))
import Value qualified as V

-- Type checking environment
type TyEnv = (DBEnv Type, FunEnv V.TClosure)

-- Helper to convert a value back to a type
valueToType :: V.Value -> Result Type
valueToType (V.VType t) = return t
valueToType V.VU = return TU -- Universe is a type
valueToType v = throw $ "Expected a type, but got value: " ++ show v

-- Convert DBType to Type for compatibility
dbTypeToType :: DBType -> Type
dbTypeToType DBTNat = TNat
dbTypeToType DBTBool = TBool
dbTypeToType DBTU = TU
dbTypeToType (DBTFun a b) = TFun (dbTypeToType a) (dbTypeToType b)
dbTypeToType (DBTDepFun a b) = TDepFun (Ident "x") (dbTypeToType a) (dbTypeToType b) -- Use dummy variable name
dbTypeToType (DBTVar _) = error "Cannot convert type variable to concrete type"
-- Phase 2: Top/Bot and pair types
dbTypeToType DBTTop = TTop
dbTypeToType DBTBot = TBot
dbTypeToType (DBTPair a b) = TPair (dbTypeToType a) (dbTypeToType b)
-- Phase 3: Identity type
dbTypeToType (DBTId a x y) = TId (dbExpToExp a) (dbExpToExp x) (dbExpToExp y)

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
infer (DBExprFun a b) env = do
  check a TU env
  check b TU env
  return TU
infer (DBExprDepFun a b) env = do
  check a TU env
  check b TU env
  return TU
-- Phase 2: Top/Bot type expressions
infer DBExprTop _ = return TU
infer DBExprBot _ = return TU
-- Phase 2: Pair type expressions
infer (DBExprPair a b) env = do
  check a TU env
  check b TU env
  return TU
-- Phase 3: Identity type
infer (DBExprId a x y) env = do
  check a TU env
  a_type_val <- interp a (emptyDB, emptyFun)
  a_type <- valueToType a_type_val
  check x a_type env
  check y a_type env
  return TU
infer DBRefl env = throw "Cannot infer type of refl, must be checked"
infer DBJ env = throw "Cannot infer type of J, must be checked"
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

-- Phase 2: Top/Bot values
infer DBTt _ = return TTop
infer DBMagic _ =
  -- magic : (A : U) -> Bot -> A (polymorphic function)
  -- For simplicity, we'll type it as Bot -> Bot for now
  -- A full implementation would need proper polymorphism
  return $ TFun TBot TBot
-- Phase 2: Boolean eliminator
infer (DBElimBool p t f b) env@(types, funs) = do
  -- Check P : bool -> U
  check p (TDepFun (Ident "_") TBool TU) env
  -- Check b : bool
  check b TBool env

  -- Evaluate P(true) to get the type for the 'then' branch
  true_branch_val <- interp (DBApp p DBTrue) (emptyDB, emptyFun)
  true_branch_type <- valueToType true_branch_val
  check t true_branch_type env

  -- Evaluate P(false) to get the type for the 'else' branch
  false_branch_val <- interp (DBApp p DBFalse) (emptyDB, emptyFun)
  false_branch_type <- valueToType false_branch_val
  check f false_branch_type env

  -- The result type is P(b)
  -- We can't know 'b' at compile time in general, but if it's a constant we can.
  -- For now, let's try to evaluate it.
  res_type_val <- interp (DBApp p b) (emptyDB, emptyFun)
  valueToType res_type_val

-- Phase 2: Pair types
infer (DBPair a b) env = do
  ta <- infer a env
  tb <- infer b env
  return $ TPair ta tb
infer (DBFst p) env = do
  tp <- infer p env
  case tp of
    TPair ta _ -> return ta
    _ -> throw "fst can only be applied to pairs"
infer (DBSnd p) env = do
  tp <- infer p env
  case tp of
    TPair _ tb -> return tb
    _ -> throw "snd can only be applied to pairs"

-- Bidirectional type checking
check :: DBExp -> Type -> TyEnv -> Result ()
-- Universe type checking - types have type U
check DBExprNat TU _ = return ()
check DBExprBool TU _ = return ()
check DBU TU _ = return ()
check (DBExprFun a b) TU env = do
  check a TU env
  check b TU env
check (DBExprDepFun a b) TU env@(types, funs) = do
  check a TU env
  check b TU env
-- Phase 2: Top/Bot type checking
check DBExprTop TU _ = return ()
check DBExprBot TU _ = return ()
-- Phase 2: Pair type checking
check (DBExprPair a b) TU env = do
  check a TU env
  check b TU env
-- Phase 3: Identity type
check (DBExprId a x y) TU env = do
  check a TU env
  a_type_val <- interp a (emptyDB, emptyFun)
  a_type <- valueToType a_type_val
  check x a_type env
  check y a_type env
check DBRefl (TId a x y) env = do
  x_val <- interp (typeToDBExp (TId a x y)) (emptyDB, emptyFun)
  y_val <- interp (typeToDBExp (TId a x y)) (emptyDB, emptyFun)
  if x_val == y_val
    then return ()
    else throw $ "Refl requires definitionally equal terms, but got " ++ show x ++ " and " ++ show y
check DBJ full_type env@(types, funs) = do
  case full_type of
    -- J : (a:U) -> (x:a) -> (p:(y:a) -> (eq:Id a x y) -> U) -> (p0:p x (refl a x)) -> (y:a) -> (eq:Id a x y) -> p y eq
    (TDepFun _ a_type (TDepFun _ x_type (TDepFun _ p_type (TDepFun _ p0_type (TDepFun _ y_type (TDepFun _ eq_type ret_type)))))) -> do
      -- This is a simplified check. A full check would be much more involved.
      -- For now, we just ensure the structure is a nested dependent function type.
      return ()
    _ -> throw "J must have a dependent function type."

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
typeToDBExp (TFun a b) = DBExprFun (typeToDBExp a) (typeToDBExp b)
typeToDBExp (TDepFun _ a b) = DBExprDepFun (typeToDBExp a) (typeToDBExp b)
typeToDBExp (TId a x y) = DBExprId (toDB [] a) (toDB [] x) (toDB [] y)
-- Phase 2: Top/Bot and pair types
typeToDBExp TTop = DBExprTop
typeToDBExp TBot = DBExprBot
typeToDBExp (TPair a b) = DBExprPair (typeToDBExp a) (typeToDBExp b)

-- Helper to convert DBExp to Exp
dbExpToExp :: DBExp -> Exp
dbExpToExp (DBVar i) = EVar (Ident ("x" ++ show i)) -- Not robust
dbExpToExp (DBFunRef f) = EVar f
dbExpToExp (DBLam e) = ELam (Ident "x") (dbExpToExp e)
dbExpToExp (DBLamAnn t e) = ELamAnn (Ident "x") (dbTypeToType t) (dbExpToExp e)
dbExpToExp DBU = EU
dbExpToExp DBExprNat = ENat
dbExpToExp DBExprBool = EBool
dbExpToExp (DBExprFun a b) = EFunType (dbExpToExp a) (dbExpToExp b)
dbExpToExp (DBExprDepFun a b) = EDepFunType (Ident "x") (dbExpToExp a) (dbExpToExp b)
dbExpToExp DBExprTop = ETop
dbExpToExp DBExprBot = EBot
dbExpToExp (DBExprPair a b) = EPairType (dbExpToExp a) (dbExpToExp b)
dbExpToExp (DBExprId a x y) = EId (dbExpToExp a) (dbExpToExp x) (dbExpToExp y)
dbExpToExp DBRefl = ERefl
dbExpToExp DBJ = EJ
dbExpToExp DBZero = EZero
dbExpToExp (DBSuc e) = ESuc (dbExpToExp e)
dbExpToExp (DBAdd e1 e2) = EAdd (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBMul e1 e2) = EMul (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp DBTrue = ETrue
dbExpToExp DBFalse = EFalse
dbExpToExp (DBNot e) = ENot (dbExpToExp e)
dbExpToExp (DBAnd e1 e2) = EAnd (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBOr e1 e2) = EOr (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBEq e1 e2) = EEq (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBLt e1 e2) = ELt (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBGt e1 e2) = EGt (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBLeq e1 e2) = ELeq (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBGeq e1 e2) = EGeq (dbExpToExp e1) (dbExpToExp e2)
dbExpToExp (DBIf c t e) = EIf (dbExpToExp c) (dbExpToExp t) (dbExpToExp e)
dbExpToExp (DBLet e body) = ELet (Ident "x") (dbExpToExp e) (dbExpToExp body)
dbExpToExp (DBApp f e) = EApp (dbExpToExp f) (dbExpToExp e)
dbExpToExp DBTt = ETt
dbExpToExp DBMagic = EMagic
dbExpToExp (DBElimBool p t f b) = EElimBool (dbExpToExp p) (dbExpToExp t) (dbExpToExp f) (dbExpToExp b)
dbExpToExp (DBPair a b) = EPair (dbExpToExp a) (dbExpToExp b)
dbExpToExp (DBFst p) = EFst (dbExpToExp p)
dbExpToExp (DBSnd p) = ESnd (dbExpToExp p)