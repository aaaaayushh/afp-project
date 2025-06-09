module Interp.DBStmt where

import DBEnv
import DeBruijn
import Evaluator
import Interp.DBExpr qualified as E
import Value

-- DE BRUIJN STATEMENT INTERPRETER ---------------------------------------------------

interp :: DBStmt -> (DBEnv Value, FunEnv Closure) -> Result (DBEnv Value, FunEnv Closure)
-- Variable declaration: val x = e
interp (DBSLet e) env@(vals, funs) = do
  val <- E.interp e env
  return (extendDB val vals, funs)

-- Type-annotated variable declaration: val x : T = e
interp (DBSLetAnn t e) env@(vals, funs) = do
  -- The type annotation doesn't affect runtime interpretation
  val <- E.interp e env
  return (extendDB val vals, funs)

-- Function declaration: fun f(x:T) = e
interp (DBSFun f t e) env@(vals, funs) =
  return (vals, bindFun f (DBFun e) funs)