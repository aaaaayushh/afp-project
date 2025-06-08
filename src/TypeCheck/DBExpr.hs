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
-- Phase 3: Vector types
dbTypeToType (DBTVec a n) = TVec (dbTypeToType a) (dbExpToExp n)
  where
    -- Convert DBExp back to Exp - this is a simplified conversion
    dbExpToExp :: DBExp -> Exp
    dbExpToExp DBZero = EZero
    dbExpToExp (DBSuc e) = ESuc (dbExpToExp e)
    dbExpToExp (DBAdd e1 e2) = EAdd (dbExpToExp e1) (dbExpToExp e2)
    dbExpToExp (DBVar i) = EVar (Ident ("x" ++ show i)) -- Convert De Bruijn index to variable name
    dbExpToExp _ = EZero -- Fallback for complex expressions

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
-- Phase 3: Vector type expressions
infer (DBExprVec a n) env = do
  check a TU env
  check n TNat env
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

-- Phase 3: Vector operations
infer DBNil env = do
  -- Empty vector [] : Vector A zero for any A
  -- Since we can't infer A, this needs type annotation in practice
  -- We could return a polymorphic type, but for simplicity we'll require annotation
  throw "Cannot infer type of empty vector. Use type annotation like: ([] : Vector nat zero)"
infer (DBCons a as) env = do
  ta <- infer a env
  -- Special case: if the second argument is an empty vector,
  -- infer its type from the first argument
  case as of
    DBNil -> return $ TVec ta (ESuc EZero) -- cons : A -> Vector A zero -> Vector A (suc zero)
    _ -> do
      tas <- infer as env
      case tas of
        TVec ta' n -> do
          -- Check that element type matches
          if ta == ta'
            then return $ TVec ta (ESuc n) -- cons : A -> Vector A n -> Vector A (suc n)
            else throw "Element type does not match vector type"
        _ -> throw "cons (::) can only be applied to vectors"
infer (DBHead v) env = do
  tv <- infer v env
  case tv of
    TVec ta n -> do
      -- Check if the length expression represents a non-zero value
      if isNonZeroLength n
        then return ta -- head : Vector A (non-zero) -> A
        else throw "Cannot take head of empty vector"
    _ -> throw "head can only be applied to vectors"
  where
    isNonZeroLength :: Exp -> Bool
    isNonZeroLength (ESuc _) = True
    isNonZeroLength (EAdd e1 e2) = isNonZeroLength e1 || isNonZeroLength e2
    isNonZeroLength EZero = False
    isNonZeroLength _ = True -- Conservative: assume non-zero for complex expressions
infer (DBTail v) env = do
  tv <- infer v env
  case tv of
    TVec ta n -> do
      -- Check if the length expression represents a non-zero value
      if isNonZeroLength n
        then return $ TVec ta (subtractOne n) -- tail : Vector A (non-zero) -> Vector A (n-1)
        else throw "Cannot take tail of empty vector"
    _ -> throw "tail can only be applied to vectors"
  where
    isNonZeroLength :: Exp -> Bool
    isNonZeroLength (ESuc _) = True
    isNonZeroLength (EAdd e1 e2) = isNonZeroLength e1 || isNonZeroLength e2
    isNonZeroLength EZero = False
    isNonZeroLength _ = True -- Conservative: assume non-zero for complex expressions
    subtractOne :: Exp -> Exp
    subtractOne (ESuc n) = n
    subtractOne (EAdd e1 e2) =
      if isNonZeroLength e1
        then EAdd (subtractOne e1) e2
        else EAdd e1 (subtractOne e2)
    subtractOne _ = EZero -- Fallback for complex expressions
infer (DBAppend v1 v2) env = do
  -- Special case handling for empty vectors
  case (v1, v2) of
    (DBNil, DBNil) -> throw "Cannot infer type of append with two empty vectors. Use type annotation."
    (DBNil, _) -> do
      tv2 <- infer v2 env
      case tv2 of
        TVec ta n -> return $ TVec ta n -- append [] v = v
        _ -> throw "append can only be applied to vectors"
    (_, DBNil) -> do
      tv1 <- infer v1 env
      case tv1 of
        TVec ta m -> return $ TVec ta m -- append v [] = v
        _ -> throw "append can only be applied to vectors"
    _ -> do
      tv1 <- infer v1 env
      tv2 <- infer v2 env
      case (tv1, tv2) of
        (TVec ta m, TVec tb n) -> do
          if ta == tb
            then return $ TVec ta (EAdd m n) -- append : Vector A m -> Vector A n -> Vector A (m + n)
            else throw "Cannot append vectors of different element types"
        _ -> throw "append can only be applied to vectors"

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
-- Phase 3: Vector type checking
check (DBExprVec a n) TU env = do
  check a TU env
  check n TNat env

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
-- Phase 2: Top/Bot and pair types
typeToDBExp TTop = DBExprTop
typeToDBExp TBot = DBExprBot
typeToDBExp (TPair a b) = DBExprPair (typeToDBExp a) (typeToDBExp b)
-- Phase 3: Vector types
typeToDBExp (TVec a n) = DBExprVec (typeToDBExp a) (toDB [] n)

-- Helper function to convert Type to DBType
typeToDBType :: Type -> DBType
typeToDBType TNat = DBTNat
typeToDBType TBool = DBTBool
typeToDBType TU = DBTU
typeToDBType (TFun a b) = DBTFun (typeToDBType a) (typeToDBType b)
typeToDBType (TDepFun _ a b) = DBTDepFun (typeToDBType a) (typeToDBType b)
-- Phase 2: Top/Bot and pair types
typeToDBType TTop = DBTTop
typeToDBType TBot = DBTBot
typeToDBType (TPair a b) = DBTPair (typeToDBType a) (typeToDBType b)
-- Phase 3: Vector types
typeToDBType (TVec a n) = DBTVec (typeToDBType a) (toDB [] n)