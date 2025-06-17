module Evaluator where

import DBEnv
  ( DBEnv,
    FunEnv,
    emptyDB,
    emptyFun,
  )
import Lang.Abs (Program)
import Lang.Par
  ( myLexer,
    pProgram,
  )

-- keeping this in for the tests
type Result a = Either String a

throw :: String -> Result a
throw = Left

type Evaluator a b = Program -> (DBEnv a, FunEnv b) -> Result a

evaluate :: (Evaluator a b, String) -> String -> Result a
evaluate (eval, errDesc) input = do
  prog <- pProgram (myLexer input)
  case eval prog (emptyDB, emptyFun) of
    Left err -> throw $ errDesc ++ " error: " ++ err
    result -> result
