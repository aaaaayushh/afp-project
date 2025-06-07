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
  | VLam Closure -- Add lambda values
  | VU -- Universe value
  | VType Type -- For types as values
  -- Phase 2: Top and Pair values
  | VTop -- unit value
  | VPair Value Value -- pair value
  deriving (Show, Eq)

data Nat
  = Zero
  | Suc Nat
  deriving (Show, Eq)

data Closure
  = DBFun DBExp -- Use De Bruijn expression for lambda body
  | NamedFun Ident -- For named functions defined in statements
  deriving (Show, Eq)

data TClosure
  = TFun Type Type
  | TDepFun Type Type -- For dependent functions in type context
  deriving (Show, Eq)

-- Helper functions for easier Nat construction
toNat :: Integer -> Nat
toNat 0 = Zero
toNat n | n > 0 = Suc (toNat (n - 1))
toNat _ = error "Cannot convert negative number to Nat"

-- Helper function to create VNat values easily
vnat :: Integer -> Value
vnat = VNat . toNat
