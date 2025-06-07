module TypeCheck.DBStmt where

import DBEnv
import DeBruijn
import Evaluator
import Lang.Abs (Ident (..), Type (..))
import TypeCheck.DBExpr qualified as E
import Value qualified as V

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

-- DE BRUIJN STATEMENT TYPE CHECKER --------------------------------------------------

infer :: DBStmt -> (DBEnv Type, FunEnv V.TClosure) -> Result (DBEnv Type, FunEnv V.TClosure)
-- Variable declaration: val x = e
infer (DBSLet e) env@(types, funs) = do
  t <- E.infer e env
  return (extendDB t types, funs)

-- Function declaration: fun f(x:T) = e
infer (DBSFun f argType e) env@(types, funs) = do
  let argTypeConc = dbTypeToType argType
  retType <- E.infer e (extendDB argTypeConc types, funs)
  return (types, bindFun f (V.TDepFun argTypeConc retType) funs)