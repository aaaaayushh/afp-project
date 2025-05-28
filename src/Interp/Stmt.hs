module Interp.Stmt where

import Env
import Evaluator
import Interp.Expr qualified as E
import Lang.Abs (Stmt (..))
import Value
  ( Closure (DBFun),
    Value,
  )

-- STATEMENT INTERPRETER -------------------------------------------------------------

interp :: Stmt -> (Env Value, Env Closure) -> Result (Env Value, Env Closure)
interp (SLet x e) env@(vars, funs) = do
  val <- E.interp e env
  return (bind x val vars, funs)
interp (SFun f x _ e) env@(vars, funs) =
  error "Old statement interpreter should not be used with De Bruijn closures"
