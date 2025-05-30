module TypeCheck.DBProg where

import DBEnv
import DeBruijn
import Evaluator
import Lang.Abs (Program (..), Stmt (..), Type)
import TypeCheck.DBExpr qualified as E
import TypeCheck.DBStmt qualified as S
import Value (TClosure)

-- Convert a list of statements to De Bruijn form for type checking
convertStmtsTC :: [Stmt] -> [DBStmt]
convertStmtsTC = convertStmts' []
  where
    convertStmts' :: Context -> [Stmt] -> [DBStmt]
    convertStmts' _ [] = []
    convertStmts' ctx (stmt : stmts) =
      let dbStmt = toDBStmt ctx stmt
          newCtx = case stmt of
            (SLet x _) -> x : ctx -- Add variable to context
            _ -> ctx -- Functions don't add to variable context
       in dbStmt : convertStmts' newCtx stmts

-- DE BRUIJN PROGRAM TYPE CHECKER ----------------------------------------------------

infer :: Program -> (DBEnv Type, FunEnv TClosure) -> Result Type
infer (Program stmts exp) env = do
  let dbStmts = convertStmtsTC stmts
      -- Build context for final expression (all bound variables in reverse order)
      varCtx = reverse [x | (SLet x _) <- stmts]
      dbExp = toDB varCtx exp
  nenv <- prepare dbStmts env
  E.infer dbExp nenv
  where
    prepare :: [DBStmt] -> (DBEnv Type, FunEnv TClosure) -> Result (DBEnv Type, FunEnv TClosure)
    prepare [] env = return env
    prepare (stmt : stmts) env = do
      nenv <- S.infer stmt env
      prepare stmts nenv