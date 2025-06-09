module Main where

import BogusTests qualified as Bogus (test)
import CustomMapTests qualified as CustomMap (test)
import DeBruijnTests qualified as DeBruijn (test)
import InterpTests qualified as Interp (test)
import Phase1Tests qualified as Phase1 (test)
import Phase2Tests qualified as Phase2 (test)
import Phase3Tests qualified as Phase3 (test)
import TypeCheckTests qualified as TypeCheck (test)

main :: IO ()
main = do
  TypeCheck.test
  Interp.test
  DeBruijn.test
  Phase1.test
  Phase2.test
  Phase3.test
  Bogus.test
  CustomMap.test
