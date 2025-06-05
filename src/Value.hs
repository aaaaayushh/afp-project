module Value where

import DeBruijn (DBExp, DBType (..))
import Lang.Abs
  ( Exp,
    Ident,
    Type,
  )

data Value
  = VNat Nat
  | VBool Bool
  | VLam Closure -- Lambda values
  | VType TypeValue -- Types as values
  deriving (Show, Eq)

data TypeValue
  = TVNat
  | TVBool
  | TVUniverse
  | TVFun TypeValue TypeValue -- Simple function type A -> B
  | TVDepFun TypeValue Closure -- Dependent function type (x : A) -> B(x)
  deriving (Show, Eq)

data Nat
  = Zero
  | Suc Nat
  deriving (Show, Eq)

data Closure
  = DBFun DBExp -- Use De Bruijn expression for lambda body
  | DBTypedFun DBType DBExp -- Typed lambda with type annotation
  | NamedFun Ident -- For named functions defined in statements
  deriving (Show, Eq)

-- Type closures for function type information
data TClosure
  = TFun Type Type -- Simple function type (legacy)
  | TDepFun TypeValue Closure -- Dependent function type with closure
  deriving (Show, Eq)

-- Helper functions for easier Nat construction
toNat :: Integer -> Nat
toNat 0 = Zero
toNat n | n > 0 = Suc (toNat (n - 1))
toNat _ = error "Cannot convert negative number to Nat"

-- Helper function to create VNat values easily
vnat :: Integer -> Value
vnat = VNat . toNat

-- Convert DBType to TypeValue (evaluation)
dbTypeToTypeValue :: DBType -> TypeValue
dbTypeToTypeValue DBTNat = TVNat
dbTypeToTypeValue DBTBool = TVBool
dbTypeToTypeValue DBTUniverse = TVUniverse
dbTypeToTypeValue (DBTFun t1 t2) = TVFun (dbTypeToTypeValue t1) (dbTypeToTypeValue t2)
dbTypeToTypeValue (DBTDepFun t1 t2) =
  -- For now, treat dependent function as simple function (ignoring dependency)
  TVFun (dbTypeToTypeValue t1) (dbTypeToTypeValue t2)
dbTypeToTypeValue (DBTVar _) = error "Cannot evaluate type variable without substitution"

-- Convert TypeValue back to DBType (quotation)
typeValueToDBType :: TypeValue -> DBType
typeValueToDBType TVNat = DBTNat
typeValueToDBType TVBool = DBTBool
typeValueToDBType TVUniverse = DBTUniverse
typeValueToDBType (TVFun t1 t2) = DBTFun (typeValueToDBType t1) (typeValueToDBType t2)
typeValueToDBType (TVDepFun t1 _) = error "Cannot quote dependent function type without evaluation context"
