module Run where

import DBEnv
import Evaluator
import Interp.DBProg (interp)
import Lang.Abs
  ( Program,
    Type,
  )
import TypeCheck.DBProg (infer)
import Value (Value)

infertype :: String -> Result Type
infertype = evaluate (infer, "Type")

run :: String -> Result Value
run input = do
  _ <- infertype input
  evaluate (interp, "Runtime") input
