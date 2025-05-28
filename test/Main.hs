module Main where

import BogusTests qualified as Bogus (test)
import DeBruijnTests qualified as DeBruijn (test)
import InterpTests qualified as Interp (test)
import TypeCheckTests qualified as TypeCheck (test)

main :: IO ()
main = do
  TypeCheck.test
  Interp.test
  DeBruijn.test
  Bogus.test
