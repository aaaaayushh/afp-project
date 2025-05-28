module TypeCheck.DBStmt where

import DBEnv
import DeBruijn
import Evaluator
import Lang.Abs (Type)
import qualified TypeCheck.DBExpr as E
import Value (TClosure (TFun))

-- DE BRUIJN STATEMENT TYPE CHECKER --------------------------------------------------

infer :: DBStmt -> (DBEnv Type, FunEnv TClosure) -> Result (DBEnv Type, FunEnv TClosure)
-- Variable declaration: val x = e
infer (DBSLet e) env@(types, funs) = do
  t <- E.infer e env
  return (extendDB t types, funs)

-- Function declaration: fun f(x:T) = e
infer (DBSFun f argType e) env@(types, funs) = do
  retType <- E.infer e (extendDB argType types, funs)
  return (types, bindFun f (TFun argType retType) funs)