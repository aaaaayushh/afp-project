# Functions Documentation

## Overview

This document provides comprehensive documentation of the key functions in the functional programming language interpreter. Functions are organized by their primary responsibilities: evaluation, type checking, De Bruijn manipulation, logging, and utility operations.

## 1. Core Evaluation Functions (`/src/Interp/`)

### Expression Evaluation (`/src/Interp/DBExpr.hs`)

#### Main Interpreter Function

```haskell
interp :: DBExp -> (DBEnv Value, FunEnv Closure) -> Result Value
```

**Purpose**: Evaluates De Bruijn expressions to runtime values

**Key Patterns**:

- Variable lookup: `DBVar i` → environment index lookup
- Lambda creation: `DBLam body` → closure creation
- Function application: `DBApp f e` → closure application with substitution
- Arithmetic: Pattern matching on `VNat` values with helper operations

**Example Cases**:

```haskell
-- Variable lookup
interp (DBVar i) (vals, _) =
  case lookupDB i vals of
    Just val -> return val
    Nothing -> throw $ "Variable index " ++ show i ++ " out of bounds"

-- Function application
interp (DBApp f e) env@(vals, funs) = do
  fval <- interp f env
  arg <- interp e env
  case fval of
    VLam (DBFun body) -> interp body (extendDB arg vals, funs)
    _ -> throw "Cannot apply non-function"
```

#### Arithmetic Helper Functions

```haskell
addNat :: Nat -> Nat -> Nat     -- Natural number addition
mulNat :: Nat -> Nat -> Nat     -- Natural number multiplication
ltNat :: Nat -> Nat -> Bool     -- Natural number comparison
eqNat :: Nat -> Nat -> Bool     -- Natural number equality
```

**Purpose**: Peano arithmetic operations on natural numbers

**Implementation**: Structural recursion on `Nat` constructors

#### Generic Operation Helpers

```haskell
arithmetic :: (DBExp, DBExp) -> (DBEnv Value, FunEnv Closure) -> (Nat -> Nat -> Nat) -> Result Value
logic :: (DBExp, DBExp) -> (DBEnv Value, FunEnv Closure) -> (Bool -> Bool -> Bool) -> Result Value
```

**Purpose**: Higher-order functions for binary operations

**Benefits**: Reduces code duplication and enforces consistent error handling

### Statement Evaluation (`/src/Interp/DBStmt.hs`)

```haskell
interpStmt :: DBStmt -> (DBEnv Value, FunEnv Closure) -> Result (DBEnv Value, FunEnv Closure)
```

**Purpose**: Processes top-level statements and updates environments

**Statement Types**:

- `DBSLet`: Value bindings that extend the value environment
- `DBSFun`: Function definitions that extend the function environment

### Program Evaluation (`/src/Interp/DBProg.hs`)

```haskell
interp :: Program -> (DBEnv Value, FunEnv Closure) -> Result Value
```

**Purpose**: Complete program evaluation pipeline

**Process**:

1. Convert program to De Bruijn form
2. Process all statements to build environments
3. Evaluate final expression in extended environment

## 2. Type Checking Functions (`/src/TypeCheck/`)

### Expression Type Checking (`/src/TypeCheck/DBExpr.hs`)

#### Main Type Inference Function

```haskell
infer :: DBExp -> (DBEnv Type, FunEnv Type) -> Result Type
```

**Purpose**: Infers types for De Bruijn expressions

**Key Cases**:

- Variables: Environment lookup
- Functions: Dependent type handling
- Applications: Type matching and substitution
- Literals: Direct type assignment

#### Type Checking Function

```haskell
check :: DBExp -> DBType -> (DBEnv Type, FunEnv Type) -> Result ()
```

**Purpose**: Verifies expressions match expected types

**Usage**: Bidirectional type checking for improved error messages

### Statement Type Checking (`/src/TypeCheck/DBStmt.hs`)

```haskell
checkStmt :: DBStmt -> (DBEnv Type, FunEnv Type) -> Result (DBEnv Type, FunEnv Type)
```

**Purpose**: Type checks statements and extends type environments

## 3. De Bruijn Manipulation Functions (`/src/DeBruijn.hs`)

### Conversion Functions

#### Expression Conversion

```haskell
toDB :: Context -> Exp -> DBExp
```

**Purpose**: Converts named expressions to De Bruijn indexed form

**Process**:

- Maintains variable name context (`Context = [Ident]`)
- Maps variable names to De Bruijn indices via list position
- Handles lambda bindings by extending context
- Preserves named function references

**Key Cases**:

```haskell
toDB ctx (EVar x) =
  case elemIndex x ctx of
    Just i -> DBVar i                    -- Convert to index
    Nothing -> DBFunRef x               -- Keep as function reference

toDB ctx (ELam x e) = DBLam (toDB (x : ctx) e)  -- Extend context
```

#### Type Conversion

```haskell
toDBType :: Context -> Type -> DBType
```

**Purpose**: Converts surface syntax types to De Bruijn form

**Handles**: Dependent types by managing variable scoping in type expressions

#### Statement Conversion

```haskell
toDBStmt :: Context -> Stmt -> DBStmt
```

**Purpose**: Converts statements while preserving function names

### Variable Manipulation Functions

#### Substitution

```haskell
subst :: Int -> DBExp -> DBExp -> DBExp
```

**Purpose**: Substitutes expression for De Bruijn variable

**Algorithm**:

- Replace target index with substitution expression
- Decrement higher indices (variable removal)
- Handle binding constructs by shifting substitution

**Example**:

```haskell
subst 0 (DBVar 1) (DBLam (DBVar 0))  -- Substitute var 1 for var 0 in lambda
-- Result: DBLam (DBVar 1)            -- with appropriate shifting
```

#### Shifting

```haskell
shiftType :: Int -> Int -> DBType -> DBType
```

**Purpose**: Adjusts De Bruijn indices when entering/exiting scopes

**Parameters**:

- `n`: Cutoff index (variables below this are not shifted)
- `k`: Shift amount (positive for entering scope, negative for exiting)

### Lens-based Traversals

#### Variable Traversal

```haskell
dbVars :: Traversal' DBExp Int
```

**Purpose**: Lens-based access to all De Bruijn indices in expressions

**Benefits**:

- Compositional variable manipulation
- Type-safe traversals
- Integration with lens ecosystem

**Usage**:

```haskell
expr & dbVars %~ (+1)  -- Increment all variable indices
```

## 4. Logging Functions (`/src/Logger.hs`)

### Logger Creation and Management

#### Logger Initialization

```haskell
newLogger :: LogLevel -> Handle -> IO Logger
startLogger :: Logger -> IO ()
stopLogger :: Logger -> IO ()
```

**Purpose**: Logger lifecycle management

**Process**:

- `newLogger`: Creates STM-based logger state
- `startLogger`: Spawns background worker thread
- `stopLogger`: Gracefully shuts down worker thread

#### Configuration Functions

```haskell
setLogLevel :: Logger -> LogLevel -> IO ()
setLogOutput :: Logger -> Handle -> IO ()
```

**Purpose**: Runtime logger configuration

**Thread Safety**: Uses STM for atomic updates

### Logging Operations

#### Core Logging Function

```haskell
logMessage :: Logger -> LogLevel -> Text -> Text -> IO ()
```

**Purpose**: Thread-safe message logging

**Process**:

1. Create timestamped log message
2. Atomically enqueue message
3. Background worker processes queue asynchronously

#### Convenience Functions

```haskell
logDebug, logInfo, logWarn, logError :: Logger -> Text -> Text -> IO ()
```

**Purpose**: Level-specific logging helpers

**Usage**: `logInfo logger "Component" "Message"`

### Background Processing

#### Logger Worker

```haskell
loggerWorker :: Logger -> IO ()
```

**Purpose**: Asynchronous log message processing

**Process**:

1. Continuously read from message queue
2. Filter by minimum log level
3. Format and output messages
4. Flush output for immediate visibility

#### Message Formatting

```haskell
formatLogMessage :: LogMessage -> Text
```

**Purpose**: Converts structured log messages to text

**Format**: `[timestamp] [level] [component] message`

## 5. Enhanced Evaluation Functions (`/src/LoggingEvaluator.hs`)

### Enhanced Evaluation Pipeline

#### Logging-Enhanced Evaluation

```haskell
evaluateWithLogging :: LoggingEvaluator -> (Evaluator a b, String) -> String -> IO (Result a)
```

**Purpose**: Adds comprehensive logging to evaluation process

**Process**:

1. Log evaluation start with input preview
2. Parse program with error logging
3. Run evaluator with success/error logging
4. Return result with timing information

#### Type Checking with Logging

```haskell
infertypeWithLogging :: LoggingEvaluator -> String -> IO (Result Type)
```

**Purpose**: Type inference with detailed logging

#### Program Execution with Logging

```haskell
runWithLogging :: LoggingEvaluator -> String -> IO (Result Value)
```

**Purpose**: Complete program execution pipeline

**Process**:

1. Type check program
2. If type checking succeeds, evaluate program
3. Log all intermediate steps
4. Return final result

## 6. Environment Functions (`/src/DBEnv.hs`)

### De Bruijn Environment Operations

```haskell
emptyDB :: DBEnv a                    -- Create empty environment
lookupDB :: Int -> DBEnv a -> Maybe a -- Index-based lookup
extendDB :: a -> DBEnv a -> DBEnv a   -- Prepend value (index 0)
```

**Purpose**: De Bruijn index management

**Implementation**: List-based with index correspondence

### Function Environment Operations

```haskell
emptyFun :: FunEnv a                           -- Create empty function environment
lookupFun :: Ident -> FunEnv a -> Maybe a     -- Name-based lookup
bindFun :: Ident -> a -> FunEnv a -> FunEnv a -- Name binding
```

**Purpose**: Named function management alongside De Bruijn indexing

## 7. Utility Functions (`/src/Value.hs`, `/src/CustomMap.hs`)

### Value Construction Helpers

```haskell
toNat :: Integer -> Nat    -- Convert Integer to Peano natural
vnat :: Integer -> Value   -- Create VNat value directly
```

**Purpose**: Convenient value creation

**Error Handling**: `toNat` throws on negative inputs

### Map Operations

```haskell
empty :: CustomMap k v                           -- Empty map
insert :: (Eq k) => k -> v -> CustomMap k v -> CustomMap k v  -- Insert/update
lookup :: (Eq k) => k -> CustomMap k v -> Maybe v            -- Lookup
```

**Purpose**: Simple key-value mapping

**Implementation**: Association list with linear search

## 8. Main Evaluation Interface (`/src/Evaluator.hs`)

### Core Evaluation Interface

```haskell
type Evaluator a b = Program -> (DBEnv a, FunEnv b) -> Result a

evaluate :: (Evaluator a b, String) -> String -> Result a
```

**Purpose**: Unified interface for evaluation operations

**Usage**: Abstracts parsing and environment setup

**Parameters**:

- `Evaluator`: Specific evaluation function (interpreter or type checker)
- `String`: Error description for context
- `String`: Input program text

## Summary

The function architecture demonstrates several key patterns:

1. **Monadic Error Handling**: Consistent use of `Result` monad
2. **Environment Threading**: De Bruijn and function environments passed through evaluation
3. **Higher-Order Abstractions**: Generic helpers for binary operations
4. **Lens Integration**: Modern Haskell techniques for data manipulation
5. **Concurrent Programming**: STM and Async for thread-safe logging
6. **Type-Driven Design**: Strong typing guides function interfaces

This comprehensive function suite provides a robust foundation for functional language interpretation with advanced features like dependent types, concurrent logging, and De Bruijn variable management.
