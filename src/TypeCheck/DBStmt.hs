module TypeCheck.DBStmt where

import DBEnv
import DeBruijn
import Evaluator
import Lang.Abs (Exp (..), Ident (..), Type (..))
import TypeCheck.DBExpr qualified as E
import Value qualified as V

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

-- DE BRUIJN STATEMENT TYPE CHECKER --------------------------------------------------

infer :: DBStmt -> (DBEnv Type, FunEnv V.TClosure) -> Result (DBEnv Type, FunEnv V.TClosure)
-- Variable declaration: val x = e
infer (DBSLet e) env@(types, funs) = do
  t <- E.infer e env
  return (extendDB t types, funs)

-- Type-annotated variable declaration: val x : T = e
infer (DBSLetAnn annotatedType e) env@(types, funs) = do
  let expectedType = dbTypeToType annotatedType
  E.check e expectedType env
  return (extendDB expectedType types, funs)

-- Function declaration: fun f(x:T) = e
infer (DBSFun f argType e) env@(types, funs) = do
  let argTypeConc = dbTypeToType argType
  retType <- E.infer e (extendDB argTypeConc types, funs)
  return (types, bindFun f (V.TDepFun argTypeConc retType) funs)