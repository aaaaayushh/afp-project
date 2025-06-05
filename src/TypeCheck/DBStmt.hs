module TypeCheck.DBStmt where

import DBEnv
import DeBruijn (DBStmt (..), dbTypeToType)
import Evaluator
import Lang.Abs (Type)
import TypeCheck.DBExpr qualified as E
import Value (TClosure (TFun))

-- DE BRUIJN STATEMENT TYPE CHECKER --------------------------------------------------

infer :: DBStmt -> (DBEnv Type, FunEnv TClosure) -> Result (DBEnv Type, FunEnv TClosure)
-- Variable declaration: val x = e
infer (DBSLet e) env@(types, funs) = do
  t <- E.infer e env
  return (extendDB t types, funs)

-- Function declaration: fun f(x:T) = e
infer (DBSFun f argType e) env@(types, funs) = do
  let argTypeConverted = dbTypeToType argType
  retType <- E.infer e (extendDB argTypeConverted types, funs)
  return (types, bindFun f (TFun argTypeConverted retType) funs)