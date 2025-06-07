module DeBruijn where

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
  | DBExprTop -- Top type as expression
  | DBExprBot -- Bot type as expression
  | DBExprPair DBExp DBExp -- pair type as expression [A, B]
  | DBExprFun DBExp DBExp -- function type as expression
  | DBExprDepFun DBExp DBExp -- dependent function type as expression
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
  -- Phase 2: Built-in types and operations
  | DBTt -- unit element
  | DBPair DBExp DBExp -- pair constructor
  | DBFst DBExp -- first projection
  | DBSnd DBExp -- second projection
  | DBMagic DBExp -- magic function
  | DBElimBool DBExp DBExp DBExp DBExp -- elimBool P t f b
  deriving (Show, Eq)

-- De Bruijn types (types can now contain variables)
data DBType
  = DBTNat
  | DBTBool
  | DBTU
  | DBTTop -- Top type
  | DBTBot -- Bot type
  | DBTPair DBType DBType -- pair type
  | DBTFun DBType DBType
  | DBTDepFun DBType DBType -- (x : a) -> b in De Bruijn form
  | DBTVar Int -- Type variable (De Bruijn index)
  deriving (Show, Eq)

data DBStmt
  = DBSLet DBExp -- val x = e (variable name removed)
  | DBSFun Ident DBType DBExp -- fun f(x:T) = e (parameter name removed, type in DB form)
  deriving (Show, Eq)

-- Context for conversion (maps variable names to De Bruijn indices)
type Context = [Ident]

-- Convert types to De Bruijn form
toDBType :: Context -> Type -> DBType
toDBType ctx TNat = DBTNat
toDBType ctx TBool = DBTBool
toDBType ctx TU = DBTU
toDBType ctx TTop = DBTTop
toDBType ctx TBot = DBTBot
toDBType ctx (TPair a b) = DBTPair (toDBType ctx a) (toDBType ctx b)
toDBType ctx (TFun a b) = DBTFun (toDBType ctx a) (toDBType ctx b)
toDBType ctx (TDepFun x a b) = DBTDepFun (toDBType ctx a) (toDBType (x : ctx) b)

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
toDB ctx ETop = DBExprTop
toDB ctx EBot = DBExprBot
toDB ctx (EPairType a b) = DBExprPair (toDB ctx a) (toDB ctx b) -- Pair type expression [A, B]
toDB ctx (EFunType a b) = DBExprFun (toDB ctx a) (toDB ctx b)
toDB ctx (EDepFunType x a b) = DBExprDepFun (toDB ctx a) (toDB (x : ctx) b)
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
-- Phase 2: Built-in types and operations
toDB ctx ETt = DBTt
toDB ctx (EPair e1 e2) = DBPair (toDB ctx e1) (toDB ctx e2)
toDB ctx (EFst e) = DBFst (toDB ctx e)
toDB ctx (ESnd e) = DBSnd (toDB ctx e)
toDB ctx (EMagic e) = DBMagic (toDB ctx e)
toDB ctx (EElimBool p t f b) = DBElimBool (toDB ctx p) (toDB ctx t) (toDB ctx f) (toDB ctx b)

-- Convert statements to De Bruijn
toDBStmt :: Context -> Stmt -> DBStmt
toDBStmt ctx (SLet x e) = DBSLet (toDB ctx e)
toDBStmt ctx (SFun f x t e) = DBSFun f (toDBType ctx t) (toDB (x : ctx) e)

-- Shift function for expressions: shift n k e
-- Increases all indices >= n by k
shift :: Int -> Int -> DBExp -> DBExp
shift n k (DBVar i)
  | i >= n = DBVar (i + k)
  | otherwise = DBVar i
shift n k (DBFunRef f) = DBFunRef f
shift n k (DBLam e) = DBLam (shift (n + 1) k e)
shift n k (DBLamAnn t e) = DBLamAnn (shiftType n k t) (shift (n + 1) k e)
shift n k DBU = DBU
-- Type expressions
shift n k DBExprNat = DBExprNat
shift n k DBExprBool = DBExprBool
shift n k DBExprTop = DBExprTop
shift n k DBExprBot = DBExprBot
shift n k (DBExprPair a b) = DBExprPair (shift n k a) (shift n k b)
shift n k (DBExprFun a b) = DBExprFun (shift n k a) (shift n k b)
shift n k (DBExprDepFun a b) = DBExprDepFun (shift n k a) (shift (n + 1) k b)
shift n k DBZero = DBZero
shift n k (DBSuc e) = DBSuc (shift n k e)
shift n k (DBAdd e1 e2) = DBAdd (shift n k e1) (shift n k e2)
shift n k (DBMul e1 e2) = DBMul (shift n k e1) (shift n k e2)
shift n k DBTrue = DBTrue
shift n k DBFalse = DBFalse
shift n k (DBNot e) = DBNot (shift n k e)
shift n k (DBAnd e1 e2) = DBAnd (shift n k e1) (shift n k e2)
shift n k (DBOr e1 e2) = DBOr (shift n k e1) (shift n k e2)
shift n k (DBEq e1 e2) = DBEq (shift n k e1) (shift n k e2)
shift n k (DBLt e1 e2) = DBLt (shift n k e1) (shift n k e2)
shift n k (DBGt e1 e2) = DBGt (shift n k e1) (shift n k e2)
shift n k (DBLeq e1 e2) = DBLeq (shift n k e1) (shift n k e2)
shift n k (DBGeq e1 e2) = DBGeq (shift n k e1) (shift n k e2)
shift n k (DBIf c t e) = DBIf (shift n k c) (shift n k t) (shift n k e)
shift n k (DBLet e body) = DBLet (shift n k e) (shift (n + 1) k body)
shift n k (DBApp f e) = DBApp (shift n k f) (shift n k e)
-- Phase 2: Built-in types and operations
shift n k DBTt = DBTt
shift n k (DBPair e1 e2) = DBPair (shift n k e1) (shift n k e2)
shift n k (DBFst e) = DBFst (shift n k e)
shift n k (DBSnd e) = DBSnd (shift n k e)
shift n k (DBMagic e) = DBMagic (shift n k e)
shift n k (DBElimBool p t f b) = DBElimBool (shift n k p) (shift n k t) (shift n k f) (shift n k b)

-- Shift function for types
shiftType :: Int -> Int -> DBType -> DBType
shiftType n k (DBTVar i)
  | i >= n = DBTVar (i + k)
  | otherwise = DBTVar i
shiftType n k DBTNat = DBTNat
shiftType n k DBTBool = DBTBool
shiftType n k DBTU = DBTU
shiftType n k DBTTop = DBTTop
shiftType n k DBTBot = DBTBot
shiftType n k (DBTPair a b) = DBTPair (shiftType n k a) (shiftType n k b)
shiftType n k (DBTFun a b) = DBTFun (shiftType n k a) (shiftType n k b)
shiftType n k (DBTDepFun a b) = DBTDepFun (shiftType n k a) (shiftType (n + 1) k b)

-- Substitution: subst n u e
-- Replace variable n with u, decrement indices > n
subst :: Int -> DBExp -> DBExp -> DBExp
subst n u (DBVar i)
  | i == n = u
  | i > n = DBVar (i - 1)
  | otherwise = DBVar i
subst n u (DBFunRef f) = DBFunRef f
subst n u (DBLam e) = DBLam (subst (n + 1) (shift 0 1 u) e)
subst n u (DBLamAnn t e) = DBLamAnn t (subst (n + 1) (shift 0 1 u) e) -- Type doesn't need substitution in this simple version
subst n u DBU = DBU
-- Type expressions
subst n u DBExprNat = DBExprNat
subst n u DBExprBool = DBExprBool
subst n u DBExprTop = DBExprTop
subst n u DBExprBot = DBExprBot
subst n u (DBExprPair a b) = DBExprPair (subst n u a) (subst n u b)
subst n u (DBExprFun a b) = DBExprFun (subst n u a) (subst n u b)
subst n u (DBExprDepFun a b) = DBExprDepFun (subst n u a) (subst (n + 1) (shift 0 1 u) b)
subst n u DBZero = DBZero
subst n u (DBSuc e) = DBSuc (subst n u e)
subst n u (DBAdd e1 e2) = DBAdd (subst n u e1) (subst n u e2)
subst n u (DBMul e1 e2) = DBMul (subst n u e1) (subst n u e2)
subst n u DBTrue = DBTrue
subst n u DBFalse = DBFalse
subst n u (DBNot e) = DBNot (subst n u e)
subst n u (DBAnd e1 e2) = DBAnd (subst n u e1) (subst n u e2)
subst n u (DBOr e1 e2) = DBOr (subst n u e1) (subst n u e2)
subst n u (DBEq e1 e2) = DBEq (subst n u e1) (subst n u e2)
subst n u (DBLt e1 e2) = DBLt (subst n u e1) (subst n u e2)
subst n u (DBGt e1 e2) = DBGt (subst n u e1) (subst n u e2)
subst n u (DBLeq e1 e2) = DBLeq (subst n u e1) (subst n u e2)
subst n u (DBGeq e1 e2) = DBGeq (subst n u e1) (subst n u e2)
subst n u (DBIf c t e) = DBIf (subst n u c) (subst n u t) (subst n u e)
subst n u (DBLet e body) = DBLet (subst n u e) (subst (n + 1) (shift 0 1 u) body)
subst n u (DBApp f e) = DBApp (subst n u f) (subst n u e)
-- Phase 2: Built-in types and operations
subst n u DBTt = DBTt
subst n u (DBPair e1 e2) = DBPair (subst n u e1) (subst n u e2)
subst n u (DBFst e) = DBFst (subst n u e)
subst n u (DBSnd e) = DBSnd (subst n u e)
subst n u (DBMagic e) = DBMagic (subst n u e)
subst n u (DBElimBool p t f b) = DBElimBool (subst n u p) (subst n u t) (subst n u f) (subst n u b)
