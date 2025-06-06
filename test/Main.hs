module Main where

import BogusTests qualified as Bogus (test)
import DeBruijnTests qualified as DeBruijn (test)
import InterpTests qualified as Interp (test)
import Phase1Tests qualified as Phase1 (test)
import TypeCheckTests qualified as TypeCheck (test)

main :: IO ()
main = do
  TypeCheck.test
  Interp.test
  DeBruijn.test
  Phase1.test
  Bogus.test
