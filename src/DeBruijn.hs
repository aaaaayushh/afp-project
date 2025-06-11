{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module DeBruijn where

import Control.Lens hiding (Context)
import Control.Lens.Plated
import Data.List (elemIndex)
import Lang.Abs (Exp (..), Ident, Stmt (..), Type (..))

-- De Bruijn expression types
data DBExp
  = DBVar Int -- De Bruijn index
  | DBFunRef Ident -- Named function reference (bypasses De Bruijn indexing)
  | DBLam DBExp -- lambda expression (parameter type omitted in DB)
  | DBLamAnn DBType DBExp -- type-annotated lambda expression
  | DBU -- Universe expression
  -- Type expressions at value level (for dependent types)
  | DBExprNat -- nat type as expression
  | DBExprBool -- bool type as expression
  | DBExprFun DBExp DBExp -- function type as expression
  | DBExprDepFun DBExp DBExp -- dependent function type as expression
  -- Phase 2: Top/Bot type expressions
  | DBExprTop -- Top type as expression
  | DBExprBot -- Bot type as expression
  | DBExprPair DBExp DBExp -- pair type as expression [A, B]
  | DBZero
  | DBSuc DBExp
  | DBAdd DBExp DBExp
  | DBMul DBExp DBExp
  | DBTrue
  | DBFalse
  | DBNot DBExp
  | DBAnd DBExp DBExp
  | DBOr DBExp DBExp
  | DBEq DBExp DBExp
  | DBLt DBExp DBExp
  | DBGt DBExp DBExp
  | DBLeq DBExp DBExp
  | DBGeq DBExp DBExp
  | DBIf DBExp DBExp DBExp
  | DBLet DBExp DBExp -- let e1 in e2 (no variable name needed)
  | DBApp DBExp DBExp -- function application (both can be arbitrary expressions now)
  -- Phase 2: Top/Bot values and eliminators
  | DBTt -- unit value tt
  | DBMagic -- magic function
  -- Phase 2: Boolean eliminator
  | DBElimBool DBExp DBExp DBExp DBExp -- elimBool P t f b
  -- Phase 2: Pair types
  | DBPair DBExp DBExp -- pair constructor (a, b)
  | DBFst DBExp -- first projection
  | DBSnd DBExp -- second projection
  -- Phase 3: Vector types
  | DBExprVec DBExp DBExp -- Vector type as expression (Vector A n)
  | DBNil -- empty vector []
  | DBCons DBExp DBExp -- cons operation (a :: as)
  | DBHead DBExp -- head function
  | DBTail DBExp -- tail function
  | DBAppend DBExp DBExp -- append function
  deriving (Show, Eq)

-- De Bruijn types (types can now contain variables)
data DBType
  = DBTNat
  | DBTBool
  | DBTU
  | DBTFun DBType DBType
  | DBTDepFun DBType DBType -- (x : a) -> b in De Bruijn form
  | DBTVar Int -- Type variable (De Bruijn index)
  -- Phase 2: Top/Bot and pair types
  | DBTTop -- Top type
  | DBTBot -- Bot type
  | DBTPair DBType DBType -- Pair type [A, B]
  -- Phase 3: Vector types
  | DBTVec DBType DBExp -- Vector type [Vector A n]
  deriving (Show, Eq)

data DBStmt
  = DBSLet DBExp -- val x = e (variable name removed)
  | DBSLetAnn DBType DBExp -- val x : T = e (variable name removed, type annotation)
  | DBSFun Ident DBType DBExp -- fun f(x:T) = e (parameter name removed, type in DB form)
  deriving (Show, Eq)

-- Context for conversion (maps variable names to De Bruijn indices)
type Context = [Ident]

-- Plated instances for automatic recursive traversal
instance Plated DBExp where
  plate f = \case
    DBVar i -> pure (DBVar i)
    DBFunRef x -> pure (DBFunRef x)
    DBLam e -> DBLam <$> f e
    DBLamAnn t e -> DBLamAnn t <$> f e
    DBU -> pure DBU
    DBExprNat -> pure DBExprNat
    DBExprBool -> pure DBExprBool
    DBExprFun a b -> DBExprFun <$> f a <*> f b
    DBExprDepFun a b -> DBExprDepFun <$> f a <*> f b
    DBExprTop -> pure DBExprTop
    DBExprBot -> pure DBExprBot
    DBExprPair a b -> DBExprPair <$> f a <*> f b
    DBZero -> pure DBZero
    DBSuc e -> DBSuc <$> f e
    DBAdd e1 e2 -> DBAdd <$> f e1 <*> f e2
    DBMul e1 e2 -> DBMul <$> f e1 <*> f e2
    DBTrue -> pure DBTrue
    DBFalse -> pure DBFalse
    DBNot e -> DBNot <$> f e
    DBAnd e1 e2 -> DBAnd <$> f e1 <*> f e2
    DBOr e1 e2 -> DBOr <$> f e1 <*> f e2
    DBEq e1 e2 -> DBEq <$> f e1 <*> f e2
    DBLt e1 e2 -> DBLt <$> f e1 <*> f e2
    DBGt e1 e2 -> DBGt <$> f e1 <*> f e2
    DBLeq e1 e2 -> DBLeq <$> f e1 <*> f e2
    DBGeq e1 e2 -> DBGeq <$> f e1 <*> f e2
    DBIf c t e -> DBIf <$> f c <*> f t <*> f e
    DBLet e body -> DBLet <$> f e <*> f body
    DBApp fn e -> DBApp <$> f fn <*> f e
    DBTt -> pure DBTt
    DBMagic -> pure DBMagic
    DBElimBool p t fn b -> DBElimBool <$> f p <*> f t <*> f fn <*> f b
    DBPair a b -> DBPair <$> f a <*> f b
    DBFst p -> DBFst <$> f p
    DBSnd p -> DBSnd <$> f p
    DBExprVec a len -> DBExprVec <$> f a <*> f len
    DBNil -> pure DBNil
    DBCons a as -> DBCons <$> f a <*> f as
    DBHead v -> DBHead <$> f v
    DBTail v -> DBTail <$> f v
    DBAppend v1 v2 -> DBAppend <$> f v1 <*> f v2

instance Plated DBType where
  plate f = \case
    DBTNat -> pure DBTNat
    DBTBool -> pure DBTBool
    DBTU -> pure DBTU
    DBTFun a b -> DBTFun <$> f a <*> f b
    DBTDepFun a b -> DBTDepFun <$> f a <*> f b
    DBTVar i -> pure (DBTVar i)
    DBTTop -> pure DBTTop
    DBTBot -> pure DBTBot
    DBTPair a b -> DBTPair <$> f a <*> f b
    DBTVec a n -> DBTVec <$> f a <*> pure n -- Note: n is DBExp, not DBType

-- Specialized traversal for shifting type variables with proper cutoff handling
shiftTypeVarsFrom :: Int -> Int -> DBType -> DBType
shiftTypeVarsFrom cutoff k = go cutoff
  where
    go :: Int -> DBType -> DBType
    go c = \case
      DBTVar i
        | i >= c -> DBTVar (i + k)
        | otherwise -> DBTVar i
      DBTDepFun a b -> DBTDepFun (go c a) (go (c + 1) b)
      other -> other & plate %~ go c

-- Specialized traversal for shifting variables with proper cutoff handling
shiftVarsFrom :: Int -> Int -> DBExp -> DBExp
shiftVarsFrom cutoff k = go cutoff
  where
    go :: Int -> DBExp -> DBExp
    go c = \case
      DBVar i
        | i >= c -> DBVar (i + k)
        | otherwise -> DBVar i
      DBLam e -> DBLam (go (c + 1) e)
      DBLamAnn t e -> DBLamAnn (shiftTypeVarsFrom cutoff k t) (go (c + 1) e)
      DBExprDepFun a b -> DBExprDepFun (go c a) (go (c + 1) b)
      DBLet e body -> DBLet (go c e) (go (c + 1) body)
      other -> other & plate %~ go c

-- Convert types to De Bruijn form
toDBType :: Context -> Type -> DBType
toDBType ctx TNat = DBTNat
toDBType ctx TBool = DBTBool
toDBType ctx TU = DBTU
toDBType ctx (TFun a b) = DBTFun (toDBType ctx a) (toDBType ctx b)
toDBType ctx (TDepFun x a b) = DBTDepFun (toDBType ctx a) (toDBType (x : ctx) b)
-- Phase 2: Top/Bot and pair types
toDBType ctx TTop = DBTTop
toDBType ctx TBot = DBTBot
toDBType ctx (TPair a b) = DBTPair (toDBType ctx a) (toDBType ctx b)
-- Phase 3: Vector types
toDBType ctx (TVec a n) = DBTVec (toDBType ctx a) (toDB ctx n)

-- Convert named expression to De Bruijn
toDB :: Context -> Exp -> DBExp
toDB ctx (EVar x) =
  case elemIndex x ctx of
    Just i -> DBVar i
    Nothing -> DBFunRef x -- Treat unbound variables as function references
toDB ctx (ELam x e) = DBLam (toDB (x : ctx) e)
toDB ctx (ELamAnn x t e) = DBLamAnn (toDBType ctx t) (toDB (x : ctx) e)
toDB ctx EU = DBU
toDB ctx ENat = DBExprNat
toDB ctx EBool = DBExprBool
toDB ctx (EFunType a b) = DBExprFun (toDB ctx a) (toDB ctx b)
toDB ctx (EDepFunType x a b) = DBExprDepFun (toDB ctx a) (toDB (x : ctx) b)
-- Phase 2: Top/Bot type expressions
toDB ctx ETop = DBExprTop
toDB ctx EBot = DBExprBot
-- Phase 2: Pair type expressions
toDB ctx (EPairType a b) = DBExprPair (toDB ctx a) (toDB ctx b)
toDB ctx EZero = DBZero
toDB ctx (ESuc e) = DBSuc (toDB ctx e)
toDB ctx (EAdd e1 e2) = DBAdd (toDB ctx e1) (toDB ctx e2)
toDB ctx (EMul e1 e2) = DBMul (toDB ctx e1) (toDB ctx e2)
toDB ctx ETrue = DBTrue
toDB ctx EFalse = DBFalse
toDB ctx (ENot e) = DBNot (toDB ctx e)
toDB ctx (EAnd e1 e2) = DBAnd (toDB ctx e1) (toDB ctx e2)
toDB ctx (EOr e1 e2) = DBOr (toDB ctx e1) (toDB ctx e2)
toDB ctx (EEq e1 e2) = DBEq (toDB ctx e1) (toDB ctx e2)
toDB ctx (ELt e1 e2) = DBLt (toDB ctx e1) (toDB ctx e2)
toDB ctx (EGt e1 e2) = DBGt (toDB ctx e1) (toDB ctx e2)
toDB ctx (ELeq e1 e2) = DBLeq (toDB ctx e1) (toDB ctx e2)
toDB ctx (EGeq e1 e2) = DBGeq (toDB ctx e1) (toDB ctx e2)
toDB ctx (EIf c t e) = DBIf (toDB ctx c) (toDB ctx t) (toDB ctx e)
toDB ctx (ELet x e body) = DBLet (toDB ctx e) (toDB (x : ctx) body)
toDB ctx (EApp f e) = DBApp (toDB ctx f) (toDB ctx e)
-- Phase 2: Top/Bot values and eliminators
toDB ctx ETt = DBTt
toDB ctx EMagic = DBMagic
-- Phase 2: Boolean eliminator
toDB ctx (EElimBool p t f b) = DBElimBool (toDB ctx p) (toDB ctx t) (toDB ctx f) (toDB ctx b)
-- Phase 2: Pair types
toDB ctx (EPair a b) = DBPair (toDB ctx a) (toDB ctx b)
toDB ctx (EFst p) = DBFst (toDB ctx p)
toDB ctx (ESnd p) = DBSnd (toDB ctx p)
-- Phase 3: Vector types
toDB ctx (EVecType a n) = DBExprVec (toDB ctx a) (toDB ctx n)
toDB ctx ENil = DBNil
toDB ctx (ECons a as) = DBCons (toDB ctx a) (toDB ctx as)
toDB ctx (EHead v) = DBHead (toDB ctx v)
toDB ctx (ETail v) = DBTail (toDB ctx v)
toDB ctx (EAppend v1 v2) = DBAppend (toDB ctx v1) (toDB ctx v2)

-- Convert statements to De Bruijn
toDBStmt :: Context -> Stmt -> DBStmt
toDBStmt ctx (SLet x e) = DBSLet (toDB ctx e)
toDBStmt ctx (SLetAnn x t e) = DBSLetAnn (toDBType ctx t) (toDB ctx e)
toDBStmt ctx (SFun f x t e) = DBSFun f (toDBType ctx t) (toDB (x : ctx) e)

-- Lens-based shift function for expressions: shift n k e
-- Increases all indices >= n by k
shift :: Int -> Int -> DBExp -> DBExp
shift = shiftVarsFrom

-- Lens-based shift function for types
shiftType :: Int -> Int -> DBType -> DBType
shiftType = shiftTypeVarsFrom

-- Lens-based substitution: subst n u e
-- Replace variable n with u, decrement indices > n
subst :: Int -> DBExp -> DBExp -> DBExp
subst n u = transformWithDepth 0
  where
    transformWithDepth :: Int -> DBExp -> DBExp
    transformWithDepth depth = \case
      DBVar i
        | i == n + depth -> shift 0 depth u
        | i > n + depth -> DBVar (i - 1)
        | otherwise -> DBVar i
      DBLam e -> DBLam (transformWithDepth (depth + 1) e)
      DBLamAnn t e -> DBLamAnn t (transformWithDepth (depth + 1) e)
      DBExprDepFun a b -> DBExprDepFun (transformWithDepth depth a) (transformWithDepth (depth + 1) b)
      DBLet e body -> DBLet (transformWithDepth depth e) (transformWithDepth (depth + 1) body)
      other -> other & plate %~ transformWithDepth depth
