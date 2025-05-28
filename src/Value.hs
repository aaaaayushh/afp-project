module Value where

import DeBruijn (DBExp)
import Lang.Abs
  ( Exp,
    Ident,
    Type,
  )

data Value
  = VNat Nat
  | VBool Bool
  deriving (Show, Eq)

data Nat
  = Zero
  | Suc Nat
  deriving (Show, Eq)

data Closure = DBFun DBExp -- Use De Bruijn expression
  deriving (Show, Eq)

data TClosure = TFun Type Type
  deriving (Show, Eq)

-- Helper functions for easier Nat construction
toNat :: Integer -> Nat
toNat 0 = Zero
toNat n | n > 0 = Suc (toNat (n - 1))
toNat _ = error "Cannot convert negative number to Nat"

-- Helper function to create VNat values easily
vnat :: Integer -> Value
vnat = VNat . toNat
