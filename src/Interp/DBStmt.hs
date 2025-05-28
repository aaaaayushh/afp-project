module Interp.DBStmt where

import DBEnv
import DeBruijn
import Evaluator
import qualified Interp.DBExpr as E
import Value

-- DE BRUIJN STATEMENT INTERPRETER ---------------------------------------------------

interp :: DBStmt -> (DBEnv Value, FunEnv Closure) -> Result (DBEnv Value, FunEnv Closure)
-- Variable declaration: val x = e
interp (DBSLet e) env@(vals, funs) = do
  val <- E.interp e env
  return (extendDB val vals, funs)

-- Function declaration: fun f(x:T) = e
interp (DBSFun f t e) env@(vals, funs) =
  return (vals, bindFun f (DBFun e) funs)