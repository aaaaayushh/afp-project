module TypeCheck.Prog where

import Env
import Evaluator
import Lang.Abs
  ( Program (Program),
    Stmt,
    Type,
  )
import Lang.ErrM qualified as S
import TypeCheck.Expr qualified as E
import TypeCheck.Stmt qualified as S
import Value (TClosure)

-- PROGRAM TYPE CHECKER --------------------------------------------------------------
-- NOTE: This is the old version - use TypeCheck.DBProg instead

infer :: Evaluator Type TClosure
infer (Program stmts exp) env =
  error "Old type checker should not be used - use TypeCheck.DBProg instead"
