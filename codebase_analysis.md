# Dependent Type Checker Implementation Analysis

This document provides a thorough line-by-line analysis of the dependent type checker implementation, explaining how each component achieves the Phase Zero and Phase One goals outlined in the project requirements.

## Project Overview

The implementation is structured as a dependently-typed functional language with the following key features:

- **De Bruijn indices** for variable representation (Phase 0 goal 2)
- **Natural numbers** instead of integers (Phase 0 goal 1)
- **Lambda expressions** and closures (Phase 0 goal 4)
- **Bidirectional type checking** (Phase 0 goal 3)
- **Dependent function types** (Phase 1)
- **Universe hierarchy** (Phase 1)

## Module Structure

The codebase is organized into several modules with clear separation of concerns:

- **Core Data Structures**: `DeBruijn.hs`, `Value.hs`
- **Environment Management**: `DBEnv.hs`, `Env.hs`
- **Interpretation**: `Interp/` directory
- **Type Checking**: `TypeCheck/` directory
- **Orchestration**: `Evaluator.hs`, `Run.hs`

---

## Core Data Structures

### DeBruijn.hs - De Bruijn Index Implementation

This module implements the core transformation from named variables to De Bruijn indices, fulfilling **Phase 0 Goal 2**.

```haskell
module DeBruijn where

import Data.List (elemIndex)
import Lang.Abs (Exp (..), Ident, Stmt (..), Type (..))
```

**Design Choice**: The module imports the abstract syntax tree from a parser (`Lang.Abs`) and transforms it into De Bruijn form. This separation allows the implementation to work with any parser that produces the expected AST.

#### Expression Data Types

```haskell
data DBExp
  = DBVar Int -- De Bruijn index
  | DBFunRef Ident -- Named function reference (bypasses De Bruijn indexing)
```

**Key Design Decision**: `DBVar Int` represents variables using De Bruijn indices where the integer indicates how many binders to skip to find the binding site. `DBFunRef Ident` preserves named function references because functions can be recursive and mutually recursive, making De Bruijn indices impractical for function names.

```haskell
  | DBLam DBExp -- lambda expression (parameter type omitted in DB)
  | DBLamAnn DBType DBExp -- type-annotated lambda expression
```

**Phase 0 Goal 4 Implementation**: These constructors implement lambda expressions. `DBLam` is an unannotated lambda (type must be inferred), while `DBLamAnn` includes explicit type annotation for bidirectional type checking.

```haskell
  | DBU -- Universe expression
  -- Type expressions at value level (for dependent types)
  | DBExprNat -- nat type as expression
  | DBExprBool -- bool type as expression
  | DBExprFun DBExp DBExp -- function type as expression
  | DBExprDepFun DBExp DBExp -- dependent function type as expression
```

**Phase 1 Implementation**: `DBU` represents the universe type. The `DBExpr*` constructors allow types to be first-class values in the language, enabling dependent types where types can depend on values. `DBExprDepFun` specifically implements dependent function types `(x : a) -> b`.

```haskell
  | DBZero
  | DBSuc DBExp
```

**Phase 0 Goal 1 Implementation**: Natural numbers with zero and successor, replacing integers. This provides a more principled foundation for dependent types.

```haskell
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
```

**Design Rationale**: All operations are preserved but variable names are eliminated where possible. `DBLet` no longer needs a variable name since De Bruijn indices handle binding. `DBApp` allows arbitrary expressions as both function and argument, supporting higher-order functions.

#### Type Data Types

```haskell
data DBType
  = DBTNat
  | DBTBool
  | DBTU
  | DBTFun DBType DBType
  | DBTDepFun DBType DBType -- (x : a) -> b in De Bruijn form
  | DBTVar Int -- Type variable (De Bruijn index)
```

**Phase 1 Implementation**: `DBTDepFun` implements dependent function types at the type level. `DBTVar` allows types to contain variables (necessary for dependent types). `DBTU` represents the universe type.

#### Context Management

```haskell
type Context = [Ident]
```

**De Bruijn Design**: Context is a list where the head represents the most recently bound variable (index 0), and each subsequent variable has index increased by 1.

#### Type Conversion Functions

```haskell
toDBType :: Context -> Type -> DBType
toDBType ctx TNat = DBTNat
toDBType ctx TBool = DBTBool
toDBType ctx TU = DBTU
toDBType ctx (TFun a b) = DBTFun (toDBType ctx a) (toDBType ctx b)
toDBType ctx (TDepFun x a b) = DBTDepFun (toDBType ctx a) (toDBType (x : ctx) b)
```

**Critical Implementation Detail**: For `TDepFun x a b`, the variable `x` is added to the context when converting `b`, ensuring that references to `x` within `b` get the correct De Bruijn index (0).

#### Expression Conversion

```haskell
toDB :: Context -> Exp -> DBExp
toDB ctx (EVar x) =
  case elemIndex x ctx of
    Just i -> DBVar i
    Nothing -> DBFunRef x -- Treat unbound variables as function references
```

**Key Algorithm**: `elemIndex` finds the position of variable `x` in the context. If found, it becomes a De Bruijn index. If not found, it's assumed to be a function reference.

**Design Decision**: Unbound variables become function references rather than errors. This allows forward references to functions and simplifies recursive function definitions.

```haskell
toDB ctx (ELam x e) = DBLam (toDB (x : ctx) e)
toDB ctx (ELamAnn x t e) = DBLamAnn (toDBType ctx t) (toDB (x : ctx) e)
```

**Lambda Handling**: When converting lambda expressions, the parameter `x` is added to the context for converting the body `e`. This ensures that references to `x` in the body get De Bruijn index 0.

```haskell
toDB ctx (EDepFunType x a b) = DBExprDepFun (toDB ctx a) (toDB (x : ctx) b)
```

**Dependent Function Types**: Similar to lambda expressions, the bound variable `x` is added to the context when converting the codomain `b`.

#### Shift Operation

```haskell
shift :: Int -> Int -> DBExp -> DBExp
shift n k (DBVar i)
  | i >= n = DBVar (i + k)
  | otherwise = DBVar i
```

**Critical De Bruijn Operation**: `shift n k expr` increases all De Bruijn indices ≥ n by k. This is essential when moving expressions under additional binders.

**Example**: If we have `λ. 0 1` (lambda with body referencing current parameter and one from outer scope), and we want to move it under another lambda, we shift: `shift 0 1 (λ. 0 1)` → `λ. 0 2` (outer reference updated).

```haskell
shift n k (DBLam e) = DBLam (shift (n + 1) k e)
shift n k (DBLamAnn t e) = DBLamAnn (shiftType n k t) (shift (n + 1) k e)
```

**Lambda Shift**: When shifting under a lambda, the cutoff `n` is increased by 1 because the lambda introduces a new binding level.

```haskell
shift n k (DBExprDepFun a b) = DBExprDepFun (shift n k a) (shift (n + 1) k b)
```

**Dependent Function Shift**: Similar to lambda expressions, dependent functions bind a variable in their codomain.

#### Substitution Operation

```haskell
subst :: Int -> DBExp -> DBExp -> DBExp
subst n u (DBVar i)
  | i == n = u
  | i > n = DBVar (i - 1)
  | otherwise = DBVar i
```

**Critical De Bruijn Operation**: `subst n u expr` replaces variable with De Bruijn index `n` with expression `u`, and decrements all indices > n by 1 (since the binding of variable `n` is being eliminated).

```haskell
subst n u (DBLam e) = DBLam (subst (n + 1) (shift 0 1 u) e)
```

**Lambda Substitution**: When substituting under a lambda:

1. Increment the target index `n` by 1 (lambda introduces new binding level)
2. Shift the substitution `u` by 1 to account for the new binding level

This is the most complex operation and getting it right is crucial for correct beta-reduction.

---

### Value.hs - Runtime Value Representation

This module defines the runtime representation of values and natural numbers.

```haskell
module Value where

import DeBruijn (DBExp)
import Lang.Abs (Exp, Ident, Type)
```

#### Value Types

```haskell
data Value
  = VNat Nat
  | VBool Bool
  | VLam Closure -- Add lambda values
  | VU -- Universe value
```

**Phase 0 Goal 1**: `VNat` uses the `Nat` type instead of `Integer`, enforcing non-negative numbers.

**Phase 0 Goal 4**: `VLam Closure` represents lambda values as first-class runtime values.

**Phase 1**: `VU` represents the universe as a runtime value, supporting `U : U`.

#### Natural Number Representation

```haskell
data Nat
  = Zero
  | Suc Nat
```

**Phase 0 Goal 1 Implementation**: This implements Peano arithmetic, the standard representation for dependent type theory. Each natural number is either zero or the successor of another natural number.

**Design Rationale**: This representation is infinite and unambiguous, making it suitable for type-level computation in dependent types.

#### Closure Representation

```haskell
data Closure
  = DBFun DBExp -- Use De Bruijn expression for lambda body
  | NamedFun Ident -- For named functions defined in statements
```

**Phase 0 Goal 4 Implementation**: Closures capture function definitions. `DBFun` stores lambda bodies using De Bruijn indices, while `NamedFun` handles named function definitions.

**Design Decision**: Separating anonymous and named functions simplifies recursion handling - named functions can refer to themselves by name, while lambda expressions use De Bruijn indices.

#### Helper Functions

```haskell
toNat :: Integer -> Nat
toNat 0 = Zero
toNat n | n > 0 = Suc (toNat (n - 1))
toNat _ = error "Cannot convert negative number to Nat"

vnat :: Integer -> Value
vnat = VNat . toNat
```

**Utility Functions**: These provide convenient conversion from Haskell integers to the internal `Nat` representation, with runtime checks to prevent negative numbers.

---

## Environment Management

### DBEnv.hs - De Bruijn Environment Operations

This module implements environment operations optimized for De Bruijn indices.

```haskell
type DBEnv a = [a]
```

**Key Design**: De Bruijn environments are lists where index 0 corresponds to the head of the list. This makes lookup O(n) but binding O(1).

```haskell
lookupDB :: Int -> DBEnv a -> Maybe a
lookupDB i env
  | i < 0 || i >= length env = Nothing
  | otherwise = Just (env !! i)
```

**Lookup Implementation**: Direct indexing into the list. The bounds check prevents runtime errors from malformed De Bruijn indices.

```haskell
extendDB :: a -> DBEnv a -> DBEnv a
extendDB x env = x : env -- prepend (index 0 is most recent)
```

**Extension Implementation**: Prepending ensures that the most recently bound variable gets index 0, matching De Bruijn convention.

```haskell
type FunEnv a = Map.Map Ident a
```

**Function Environment**: Functions still use name-based lookup since they need to support recursion and forward references.

---

## Expression Interpretation

### Interp/DBExpr.hs - Expression Evaluation

This module implements the evaluation semantics for De Bruijn expressions.

```haskell
interp :: DBExp -> (DBEnv Value, FunEnv Closure) -> Result Value
```

**Evaluation Function**: Takes a De Bruijn expression and environments for variables and functions, returns a value.

#### Variable and Function Reference Evaluation

```haskell
interp (DBVar i) (vals, _) =
  case lookupDB i vals of
    Just val -> return val
    Nothing -> throw $ "Variable index " ++ show i ++ " out of bounds"
```

**De Bruijn Variable Lookup**: Direct lookup in the value environment using the De Bruijn index.

```haskell
interp (DBFunRef f) (_, funs) =
  case lookupFun f funs of
    Just closure -> return $ VLam closure
    Nothing -> throw $ "Function " ++ show f ++ " not found"
```

**Function Reference**: Named functions are looked up in the function environment and wrapped as lambda values.

#### Type Expression Evaluation

```haskell
interp DBU _ = return VU
interp DBExprNat _ = return VU
interp DBExprBool _ = return VU
interp (DBExprFun a b) env = return VU
interp (DBExprDepFun a b) env = return VU
```

**Phase 1 Implementation**: All type expressions evaluate to the universe value `VU`. This implements the universe hierarchy where `nat : U`, `bool : U`, `(a -> b) : U`, etc.

**Design Decision**: The evaluator doesn't fully evaluate type expressions - it just confirms they're well-formed types by returning `VU`.

#### Natural Number Evaluation

```haskell
interp DBZero _ = return $ VNat Zero
interp (DBSuc e) env = do
  v <- interp e env
  case v of
    VNat n -> return $ VNat (Suc n)
    _ -> throw "Successor can only be applied to natural numbers"
```

**Phase 0 Goal 1**: Natural number evaluation with runtime type checking to ensure successor is only applied to natural numbers.

#### Arithmetic Operations

```haskell
arithmetic :: (DBExp, DBExp) -> (DBEnv Value, FunEnv Closure) -> (Nat -> Nat -> Nat) -> Result Value
arithmetic (e1, e2) env f = do
  v1 <- interp e1 env
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VNat (f n1 n2)
    _ -> throw "Arithmetic can only be performed on natural numbers"
```

**Helper Function**: Abstracts the pattern of evaluating two expressions and applying a natural number operation with runtime type checking.

#### Lambda Expression Evaluation

```haskell
interp (DBLam body) env = return $ VLam (DBFun body)
interp (DBLamAnn _ body) env = return $ VLam (DBFun body)
```

**Phase 0 Goal 4**: Lambda expressions evaluate to closure values. Type annotations are ignored at runtime (they're only used during type checking).

#### Function Application

```haskell
interp (DBApp f e) env@(vals, funs) = do
  fval <- interp f env
  arg <- interp e env
  case fval of
    VLam (DBFun body) -> do
      -- Apply lambda: substitute argument for De Bruijn index 0
      interp body (extendDB arg vals, funs)
    VLam (NamedFun fname) -> do
      -- Look up named function and apply
      case lookupFun fname funs of
        Just (DBFun body) ->
          interp body (extendDB arg vals, funs)
        Nothing -> throw $ "Named function " ++ show fname ++ " not found"
    _ -> throw "Cannot apply non-function"
```

**Beta-Reduction Implementation**: This is the core of lambda calculus evaluation. When applying a lambda to an argument:

1. Evaluate the function expression to get a closure
2. Evaluate the argument
3. For `DBFun body`: extend the environment with the argument value and evaluate the body
4. For `NamedFun`: look up the function definition and apply similarly

**Key Insight**: The De Bruijn representation makes substitution implicit - instead of explicitly substituting the argument into the function body, we extend the environment and let variable lookup handle it.

#### Let Expression Evaluation

```haskell
interp (DBLet e body) env@(vals, funs) = do
  val <- interp e env
  interp body (extendDB val vals, funs)
```

**Let Binding**: Evaluates the bound expression and extends the environment for evaluating the body. This is essentially the same as function application - let expressions are syntactic sugar for lambda application.

---

## Type Checking

### TypeCheck/DBExpr.hs - Bidirectional Type Checking

This module implements bidirectional type checking, fulfilling **Phase 0 Goal 3**.

```haskell
type TyEnv = (DBEnv Type, FunEnv V.TClosure)
```

**Type Environment**: Separates variable types (using De Bruijn indices) from function types (using names).

#### Type Inference

```haskell
infer :: DBExp -> TyEnv -> Result Type
```

**Phase 0 Goal 3**: The `infer` function attempts to determine the type of an expression without additional context.

#### Variable and Function Inference

```haskell
infer (DBVar i) (types, _) =
  case lookupDB i types of
    Just t -> return t
    Nothing -> throw $ "Variable index " ++ show i ++ " out of bounds"
```

**Variable Type Inference**: Direct lookup using De Bruijn index in the type environment.

```haskell
infer (DBFunRef f) (_, funs) =
  case lookupFun f funs of
    Just tclosure -> return $ case tclosure of
      V.TFun argType retType -> TFun argType retType
      V.TDepFun argType retType -> TDepFun (Ident "x") argType retType
    Nothing -> throw $ "Function " ++ show f ++ " not found"
```

**Function Type Inference**: Functions have their types stored in the function environment. Note the dummy variable name for dependent function types.

#### Lambda Type Checking

```haskell
infer (DBLam _) _ = throw "Cannot infer type of lambda expression without annotation"
```

**Phase 0 Goal 3 - Bidirectional Design**: Unannotated lambdas cannot have their types inferred - they must be checked against an expected type. This is a key principle of bidirectional type checking.

```haskell
infer (DBLamAnn domType body) env@(types, funs) = do
  let domTypeConc = dbTypeToType domType
  codType <- infer body (extendDB domTypeConc types, funs)
  return $ TDepFun (Ident "x") domTypeConc codType
```

**Annotated Lambda Inference**: The domain type is given, so we can extend the environment and infer the codomain type, producing a dependent function type.

#### Universe Type Checking

```haskell
infer DBU _ = return TU
infer DBExprNat _ = return TU
infer DBExprBool _ = return TU
infer (DBExprFun a b) env = do
  check a TU env
  check b TU env
  return TU
infer (DBExprDepFun a b) env = do
  check a TU env
  check b TU env
  return TU
```

**Phase 1 Implementation**: This implements the universe hierarchy:

- `U : U` (though this leads to inconsistency, as noted in the goals)
- `nat : U` and `bool : U`
- `(a -> b) : U` when `a : U` and `b : U`
- `(x : a) -> b : U` when `a : U` and `b : U`

#### Function Application Type Checking

```haskell
infer (DBApp f e) env = do
  fType <- infer f env
  case fType of
    TFun targ tret -> do
      check e targ env
      return tret
    TDepFun _ targ tret -> do
      check e targ env
      return tret -- For now, don't do dependent substitution
    _ -> throw "Cannot apply non-function"
```

**Function Application**: Infers the function type, checks that the argument has the expected domain type, and returns the codomain type.

**Limitation**: The comment "For now, don't do dependent substitution" indicates that this implementation doesn't fully handle dependent types where the return type depends on the argument value. This would require evaluation during type checking.

#### Type Checking

```haskell
check :: DBExp -> Type -> TyEnv -> Result ()
```

**Phase 0 Goal 3**: The `check` function verifies that an expression has a specific expected type.

#### Lambda Type Checking

```haskell
check (DBLam body) (TFun targ tret) env@(types, funs) = do
  check body tret (extendDB targ types, funs)
check (DBLam body) (TDepFun _ targ tret) env@(types, funs) = do
  check body tret (extendDB targ types, funs)
```

**Bidirectional Lambda Checking**: When checking a lambda against a function type, we extend the type environment with the domain type and check the body against the codomain type.

```haskell
check e expected env = do
  actual <- infer e env
  if actual == expected
    then return ()
    else throw $ "Type mismatch: expected " ++ show expected ++ " but got " ++ show actual
```

**Fallback Rule**: For expressions that can be inferred, check by inferring and comparing.

---

## Statement Processing

### Interp/DBStmt.hs & TypeCheck/DBStmt.hs - Statement Handling

These modules handle let bindings and function definitions.

#### Statement Interpretation

```haskell
interp (DBSLet e) env@(vals, funs) = do
  val <- E.interp e env
  return (extendDB val vals, funs)
```

**Let Statement**: Evaluates the expression and extends the variable environment.

```haskell
interp (DBSFun f t e) env@(vals, funs) =
  return (vals, bindFun f (DBFun e) funs)
```

**Function Definition**: Adds the function body to the function environment without modifying the variable environment.

#### Statement Type Checking

```haskell
infer (DBSLet e) env@(types, funs) = do
  t <- E.infer e env
  return (extendDB t types, funs)
```

**Let Statement Type Checking**: Infers the type of the expression and extends the type environment.

```haskell
infer (DBSFun f argType e) env@(types, funs) = do
  let argTypeConc = dbTypeToType argType
  retType <- E.infer e (extendDB argTypeConc types, funs)
  return (types, bindFun f (V.TDepFun argTypeConc retType) funs)
```

**Function Definition Type Checking**: Extends the type environment with the parameter type, infers the return type, and stores the function's type as a dependent function type.

---

## Program Processing

### DBProg.hs - Complete Program Handling

These modules orchestrate the processing of complete programs.

#### Context Building for De Bruijn Conversion

```haskell
convertStmts :: [Stmt] -> [DBStmt]
convertStmts = convertStmts' []
  where
    convertStmts' :: Context -> [Stmt] -> [DBStmt]
    convertStmts' _ [] = []
    convertStmts' ctx (stmt : stmts) =
      let dbStmt = toDBStmt ctx stmt
          newCtx = case stmt of
            (SLet x _) -> x : ctx -- Add variable to context
            _ -> ctx -- Functions don't add to variable context
       in dbStmt : convertStmts' newCtx stmts
```

**Critical Algorithm**: This builds the De Bruijn context incrementally as statements are processed. Let statements add variables to the context, while function definitions don't (functions use name-based lookup).

#### Final Expression Processing

```haskell
interp (Program stmts exp) env = do
  let dbStmts = convertStmts stmts
      -- Build context for final expression (all bound variables in reverse order)
      varCtx = reverse [x | (SLet x _) <- stmts]
      dbExp = toDB varCtx exp
  nenv <- prepare dbStmts env
  E.interp dbExp nenv
```

**Key Insight**: The context for the final expression includes all variables bound by let statements in reverse order (most recent first), which matches the De Bruijn convention.

#### Environment Preparation

```haskell
prepare :: [DBStmt] -> (DBEnv Value, FunEnv Closure) -> Result (DBEnv Value, FunEnv Closure)
prepare [] env = return env
prepare (stmt : stmts) env = do
  nenv <- S.interp stmt env
  prepare stmts nenv
```

**Sequential Processing**: Statements are processed sequentially, each potentially modifying the environment for subsequent statements.

---

## Orchestration

### Run.hs - Main Entry Point

```haskell
infertype :: String -> Result Type
infertype = evaluate (infer, "Type")

run :: String -> Result Value
run input = do
  _ <- infertype input
  evaluate (interp, "Runtime") input
```

**Two-Phase Execution**: Programs are first type-checked, then executed only if type checking succeeds. This ensures type safety.

### Evaluator.hs - Evaluation Framework

```haskell
type Evaluator a b = Program -> (DBEnv a, FunEnv b) -> Result a

evaluate :: (Evaluator a b, String) -> String -> Result a
evaluate (eval, errDesc) input = do
  prog <- pProgram (myLexer input)
  case eval prog (emptyDB, emptyFun) of
    Left err -> throw $ errDesc ++ " error: " ++ err
    result -> result
```

**Generic Evaluation Framework**: Abstracts the pattern of parsing input and running an evaluator with error handling.

---

## Achievement of Phase Goals

### Phase Zero Goals

1. **✅ Convert integers to natural numbers**: Implemented via `Nat` type with `Zero` and `Suc` constructors
2. **✅ De Bruijn indices**: Complete implementation with `shift` and `subst` operations
3. **✅ Bidirectional type checking**: `infer` and `check` functions with appropriate lambda handling
4. **✅ Lambda expressions**: First-class lambda values with proper closure semantics

### Phase One Goals

1. **✅ Dependent function type `(x : a) -> b`**: Implemented as `DBTDepFun` and `DBExprDepFun`
2. **✅ Lambda expressions `\x -> u`**: Implemented as `DBLam` and `DBLamAnn`
3. **✅ Universe type `U`**: Implemented with type expressions evaluating to `VU`
4. **⚠️ Type evaluation during checking**: Basic implementation present but dependent substitution not fully implemented

### Design Strengths

1. **Clean separation of concerns**: Distinct modules for syntax, evaluation, and type checking
2. **Principled De Bruijn implementation**: Proper handling of variable binding with shift/subst operations
3. **Bidirectional type checking**: Enables inference where possible, checking where necessary
4. **Type-safe evaluation**: Two-phase execution ensures runtime type safety

### Limitations and Areas for Improvement

1. **Incomplete dependent substitution**: The type checker doesn't perform substitution in dependent types during application
2. **Universe inconsistency**: `U : U` leads to logical inconsistency (noted in goals)
3. **Limited dependent features**: No dependent elimination principles or indexed types
4. **Error handling**: Error messages could be more informative with location information

This implementation provides a solid foundation for a dependent type checker, successfully implementing the core features required for Phase Zero and Phase One while maintaining clean, understandable code structure.
