module Interp.DBProg where

import DBEnv
import DeBruijn
import Evaluator
import Interp.DBExpr qualified as E
import Interp.DBStmt qualified as S
import Lang.Abs (Program (..), Stmt (..))
import Value

-- Convert a list of statements to De Bruijn form
convertStmts :: [Stmt] -> [DBStmt]
convertStmts = convertStmts' []
  where
    convertStmts' :: Context -> [Stmt] -> [DBStmt]
    convertStmts' _ [] = []
    convertStmts' ctx (stmt : stmts) =
      let dbStmt = toDBStmt ctx stmt
          newCtx = case stmt of
            (SLet x _) -> x : ctx -- Add variable to context
            (SLetAnn x _ _) -> x : ctx -- Add type-annotated variable to context
            _ -> ctx -- Functions don't add to variable context
       in dbStmt : convertStmts' newCtx stmts

-- DE BRUIJN PROGRAM INTERPRETER

interp :: Program -> (DBEnv Value, FunEnv Closure) -> Result Value
interp (Program stmts exp) env = do
  let dbStmts = convertStmts stmts
      -- Build context for final expression (all bound variables in reverse order)
      varCtx =
        reverse
          [ x | stmt <- stmts, x <- case stmt of
                                 SLet x _ -> [x]
                                 SLetAnn x _ _ -> [x]
                                 _ -> []
          ]
      dbExp = toDB varCtx exp
  nenv <- prepare dbStmts env
  E.interp dbExp nenv
  where
    prepare :: [DBStmt] -> (DBEnv Value, FunEnv Closure) -> Result (DBEnv Value, FunEnv Closure)
    prepare [] env = return env
    prepare (stmt : stmts) env = do
      nenv <- S.interp stmt env
      prepare stmts nenv