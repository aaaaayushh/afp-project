module DeBruijn where

import Data.List (elemIndex)
import Lang.Abs (Exp (..), Ident, Stmt (..), Type)

-- De Bruijn expression types
data DBExp
  = DBVar Int -- De Bruijn index
  | DBFunRef Ident -- Named function reference (bypasses De Bruijn indexing)
  | DBLam DBExp -- lambda expression (parameter type omitted in DB)
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
  deriving (Show, Eq)

data DBStmt
  = DBSLet DBExp -- val x = e (variable name removed)
  | DBSFun Ident Type DBExp -- fun f(x:T) = e (parameter name removed)
  deriving (Show, Eq)

-- Context for conversion (maps variable names to De Bruijn indices)
type Context = [Ident]

-- Convert named expression to De Bruijn
toDB :: Context -> Exp -> DBExp
toDB ctx (EVar x) =
  case elemIndex x ctx of
    Just i -> DBVar i
    Nothing -> DBFunRef x -- Treat unbound variables as function references
toDB ctx (ELam x e) = DBLam (toDB (x : ctx) e)
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

-- Convert statements to De Bruijn
toDBStmt :: Context -> Stmt -> DBStmt
toDBStmt ctx (SLet x e) = DBSLet (toDB ctx e)
toDBStmt ctx (SFun f x t e) = DBSFun f t (toDB (x : ctx) e)

-- Shift function: shift n k e
-- Increases all indices >= n by k
shift :: Int -> Int -> DBExp -> DBExp
shift n k (DBVar i)
  | i >= n = DBVar (i + k)
  | otherwise = DBVar i
shift n k (DBFunRef f) = DBFunRef f
shift n k (DBLam e) = DBLam (shift (n + 1) k e)
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

-- Substitution: subst n u e
-- Replace variable n with u, decrement indices > n
subst :: Int -> DBExp -> DBExp -> DBExp
subst n u (DBVar i)
  | i == n = u
  | i > n = DBVar (i - 1)
  | otherwise = DBVar i
subst n u (DBFunRef f) = DBFunRef f
subst n u (DBLam e) = DBLam (subst (n + 1) (shift 0 1 u) e)
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
