module DeBruijn where

import Control.Lens hiding (Context)
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

-- Shift function for types
shiftType :: Int -> Int -> DBType -> DBType
shiftType n k (DBTVar i)
  | i >= n = DBTVar (i + k)
  | otherwise = DBTVar i
shiftType n k DBTNat = DBTNat
shiftType n k DBTBool = DBTBool
shiftType n k DBTU = DBTU
shiftType n k (DBTFun a b) = DBTFun (shiftType n k a) (shiftType n k b)
shiftType n k (DBTDepFun a b) = DBTDepFun (shiftType n k a) (shiftType (n + 1) k b)
-- Phase 2: Top/Bot and pair types
shiftType n k DBTTop = DBTTop
shiftType n k DBTBot = DBTBot
shiftType n k (DBTPair a b) = DBTPair (shiftType n k a) (shiftType n k b)
-- Phase 3: Vector types
shiftType n k (DBTVec a len) = DBTVec (shiftType n k a) (shiftLens n k len)

-- Substitution: subst n u e
-- Replace variable n with u, decrement indices > n
subst :: Int -> DBExp -> DBExp -> DBExp
subst n u (DBVar i)
  | i == n = u
  | i > n = DBVar (i - 1)
  | otherwise = DBVar i
subst n u (DBFunRef f) = DBFunRef f
subst n u (DBLam e) = DBLam (subst (n + 1) (shiftLens 0 1 u) e)
subst n u (DBLamAnn t e) = DBLamAnn t (subst (n + 1) (shiftLens 0 1 u) e) -- Type doesn't need substitution in this simple version
subst n u DBU = DBU
-- Type expressions
subst n u DBExprNat = DBExprNat
subst n u DBExprBool = DBExprBool
subst n u (DBExprFun a b) = DBExprFun (subst n u a) (subst n u b)
subst n u (DBExprDepFun a b) = DBExprDepFun (subst n u a) (subst (n + 1) (shiftLens 0 1 u) b)
-- Phase 2: Top/Bot type expressions
subst n u DBExprTop = DBExprTop
subst n u DBExprBot = DBExprBot
subst n u (DBExprPair a b) = DBExprPair (subst n u a) (subst n u b)
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
subst n u (DBLet e body) = DBLet (subst n u e) (subst (n + 1) (shiftLens 0 1 u) body)
subst n u (DBApp f e) = DBApp (subst n u f) (subst n u e)
-- Phase 2: Top/Bot values and eliminators
subst n u DBTt = DBTt
subst n u DBMagic = DBMagic
-- Phase 2: Boolean eliminator
subst n u (DBElimBool p t f b) = DBElimBool (subst n u p) (subst n u t) (subst n u f) (subst n u b)
-- Phase 2: Pair types
subst n u (DBPair a b) = DBPair (subst n u a) (subst n u b)
subst n u (DBFst p) = DBFst (subst n u p)
subst n u (DBSnd p) = DBSnd (subst n u p)
-- Phase 3: Vector types
subst n u (DBExprVec a len) = DBExprVec (subst n u a) (subst n u len)
subst n u DBNil = DBNil
subst n u (DBCons a as) = DBCons (subst n u a) (subst n u as)
subst n u (DBHead v) = DBHead (subst n u v)
subst n u (DBTail v) = DBTail (subst n u v)
subst n u (DBAppend v1 v2) = DBAppend (subst n u v1) (subst n u v2)

-- Lens-based approach for accessing De Bruijn variables
-- Traversal that focuses on all DBVar indices in an expression
dbVars :: Traversal' DBExp Int
dbVars f (DBVar i) = DBVar <$> f i
dbVars f (DBLam e) = DBLam <$> dbVars f e
dbVars f (DBLamAnn t e) = DBLamAnn t <$> dbVars f e
dbVars f (DBExprFun a b) = DBExprFun <$> dbVars f a <*> dbVars f b
dbVars f (DBExprDepFun a b) = DBExprDepFun <$> dbVars f a <*> dbVars f b
dbVars f (DBExprPair a b) = DBExprPair <$> dbVars f a <*> dbVars f b
dbVars f (DBSuc e) = DBSuc <$> dbVars f e
dbVars f (DBAdd e1 e2) = DBAdd <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBMul e1 e2) = DBMul <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBNot e) = DBNot <$> dbVars f e
dbVars f (DBAnd e1 e2) = DBAnd <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBOr e1 e2) = DBOr <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBEq e1 e2) = DBEq <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBLt e1 e2) = DBLt <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBGt e1 e2) = DBGt <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBLeq e1 e2) = DBLeq <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBGeq e1 e2) = DBGeq <$> dbVars f e1 <*> dbVars f e2
dbVars f (DBIf c t e) = DBIf <$> dbVars f c <*> dbVars f t <*> dbVars f e
dbVars f (DBLet e body) = DBLet <$> dbVars f e <*> dbVars f body
dbVars f (DBApp fun arg) = DBApp <$> dbVars f fun <*> dbVars f arg
dbVars f (DBElimBool p t fun b) = DBElimBool <$> dbVars f p <*> dbVars f t <*> dbVars f fun <*> dbVars f b
dbVars f (DBPair a b) = DBPair <$> dbVars f a <*> dbVars f b
dbVars f (DBFst p) = DBFst <$> dbVars f p
dbVars f (DBSnd p) = DBSnd <$> dbVars f p
dbVars f (DBExprVec a len) = DBExprVec <$> dbVars f a <*> dbVars f len
dbVars f (DBCons a as) = DBCons <$> dbVars f a <*> dbVars f as
dbVars f (DBHead v) = DBHead <$> dbVars f v
dbVars f (DBTail v) = DBTail <$> dbVars f v
dbVars f (DBAppend v1 v2) = DBAppend <$> dbVars f v1 <*> dbVars f v2
-- Base cases that don't contain variables
dbVars _ e = pure e

-- Lens-based shift function (respects De Bruijn scoping)
shiftLens :: Int -> Int -> DBExp -> DBExp
shiftLens n k (DBVar i)
  | i >= n = DBVar (i + k)
  | otherwise = DBVar i
shiftLens n k (DBLam e) = DBLam (shiftLens (n + 1) k e)
shiftLens n k (DBLamAnn t e) = DBLamAnn (shiftType n k t) (shiftLens (n + 1) k e)
shiftLens n k (DBLet e body) = DBLet (shiftLens n k e) (shiftLens (n + 1) k body)
shiftLens n k e = e & dbVars %~ (\i -> if i >= n then i + k else i)

-- Lenses for accessing components of composite expressions
lamAnnType :: Lens' DBExp DBType
lamAnnType = lens get set
  where
    get (DBLamAnn t _) = t
    get _ = error "Not a DBLamAnn"
    set (DBLamAnn _ e) t = DBLamAnn t e
    set _ _ = error "Not a DBLamAnn"

lamAnnBody :: Lens' DBExp DBExp
lamAnnBody = lens get set
  where
    get (DBLamAnn _ e) = e
    get _ = error "Not a DBLamAnn"
    set (DBLamAnn t _) e = DBLamAnn t e
    set _ _ = error "Not a DBLamAnn"

-- Traversal for type variables in DBType
typeVars :: Traversal' DBType Int
typeVars f (DBTVar i) = DBTVar <$> f i
typeVars f (DBTFun a b) = DBTFun <$> typeVars f a <*> typeVars f b
typeVars f (DBTDepFun a b) = DBTDepFun <$> typeVars f a <*> typeVars f b
typeVars f (DBTPair a b) = DBTPair <$> typeVars f a <*> typeVars f b
typeVars f (DBTVec a len) = DBTVec <$> typeVars f a <*> pure len
typeVars _ t = pure t

-- Helper function using lenses: extract all variable indices from an expression
getAllVarIndices :: DBExp -> [Int]
getAllVarIndices = toListOf dbVars

-- Helper function using lenses: count number of variables
countVars :: DBExp -> Int
countVars = lengthOf dbVars
