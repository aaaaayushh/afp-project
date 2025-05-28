module Interp.Prog where

import Env
import Evaluator
import Interp.Expr qualified as E
import Interp.Stmt qualified as S
import Lang.Abs
  ( Program (Program),
    Stmt,
  )
import Value
  ( Closure,
    Value,
  )

-- PROGRAM INTERPRETER ---------------------------------------------------------------
-- NOTE: This is the old version - use Interp.DBProg instead

interp :: Evaluator Value Closure
interp (Program stmts exp) env =
  error "Old interpreter should not be used - use Interp.DBProg instead"
