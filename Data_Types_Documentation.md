# Data Types Documentation

## Overview

This document provides comprehensive documentation of all data types used in the functional programming language interpreter. The type system includes runtime values, De Bruijn expressions, logging infrastructure, and abstract syntax representations.

## 1. Core Value System (`/src/Value.hs`)

### Value Type

```haskell
data Value
  = VNat Nat              -- Natural numbers
  | VBool Bool            -- Boolean values
  | VLam Closure          -- Lambda functions
  | VU                    -- Universe value
  | VType Type            -- Types as first-class values
  | VTop                  -- Unit value (Top type)
  | VPair Value Value     -- Pair values
  | VVec [Value]          -- Vector values
  deriving (Show, Eq)
```

**Purpose**: Represents all runtime values in the language interpreter

**Key Variants**:

- **VNat**: Natural numbers
- **VBool**: Standard boolean values (True/False)
- **VLam**: Function values with closures
- **VU**: Universe type for type-level programming
- **VType**: Types as values (supports dependent types)
- **VTop**: Unit value for the Top type
- **VPair**: Ordered pairs for product types
- **VVec**: Vectors as lists of values

### Natural Numbers

```haskell
data Nat
  = Zero                  -- Zero natural number
  | Suc Nat              -- Successor of a natural number
  deriving (Show, Eq)
```

**Benefits**:

- Guarantees non-negative integers
- Structural recursion for arithmetic operations
- Clear inductive structure for proofs

**Helper Functions**:

```haskell
toNat :: Integer -> Nat           -- Convert Integer to Nat
vnat :: Integer -> Value          -- Create VNat value directly
```

### Closures

```haskell
data Closure
  = DBFun DBExp           -- De Bruijn lambda body
  | NamedFun Ident        -- Named function reference
  deriving (Show, Eq)
```

**Purpose**: Represents function values with their execution contexts

**Variants**:

- **DBFun**: Anonymous functions with De Bruijn indexed bodies
- **NamedFun**: Named functions defined in statements

### Type Closures

```haskell
data TClosure
  = TFun Type Type        -- Simple function type
  | TDepFun Type Type     -- Dependent function type
  deriving (Show, Eq)
```

**Purpose**: Type-level closures for dependent type checking

## 2. De Bruijn Expression System (`/src/DeBruijn.hs`)

### De Bruijn Expressions

```haskell
data DBExp
  = DBVar Int                    -- De Bruijn variable index
  | DBFunRef Ident              -- Named function reference
  | DBLam DBExp                 -- Lambda expression
  | DBLamAnn DBType DBExp       -- Type-annotated lambda
  | DBU                         -- Universe expression
  -- Type expressions
  | DBExprNat                   -- Nat type as expression
  | DBExprBool                  -- Bool type as expression
  | DBExprFun DBExp DBExp       -- Function type expression
  | DBExprDepFun DBExp DBExp    -- Dependent function type
  | DBExprTop                   -- Top type expression
  | DBExprBot                   -- Bot type expression
  | DBExprPair DBExp DBExp      -- Pair type expression
  -- Natural number operations
  | DBZero                      -- Zero literal
  | DBSuc DBExp                 -- Successor function
  | DBAdd DBExp DBExp           -- Addition
  | DBMul DBExp DBExp           -- Multiplication
  -- Boolean operations
  | DBTrue                      -- True literal
  | DBFalse                     -- False literal
  | DBNot DBExp                 -- Boolean negation
  | DBAnd DBExp DBExp           -- Boolean conjunction
  | DBOr DBExp DBExp            -- Boolean disjunction
  -- Comparison operations
  | DBEq DBExp DBExp            -- Equality
  | DBLt DBExp DBExp            -- Less than
  | DBGt DBExp DBExp            -- Greater than
  | DBLeq DBExp DBExp           -- Less than or equal
  | DBGeq DBExp DBExp           -- Greater than or equal
  -- Control flow
  | DBIf DBExp DBExp DBExp      -- Conditional expression
  | DBLet DBExp DBExp           -- Let binding
  | DBApp DBExp DBExp           -- Function application
  -- Advanced features
  | DBTt                        -- Unit value
  | DBMagic                     -- Magic elimination function
  | DBElimBool DBExp DBExp DBExp DBExp  -- Boolean eliminator
  | DBPair DBExp DBExp          -- Pair constructor
  | DBFst DBExp                 -- First projection
  | DBSnd DBExp                 -- Second projection
  -- Vector operations
  | DBExprVec DBExp DBExp       -- Vector type expression
  | DBNil                       -- Empty vector
  | DBCons DBExp DBExp          -- Vector cons
  | DBHead DBExp                -- Vector head
  | DBTail DBExp                -- Vector tail
  | DBAppend DBExp DBExp        -- Vector append
  deriving (Show, Eq)
```

**Purpose**: De Bruijn indexed expressions for variable management

**Key Features**:

- **Variable Indices**: `DBVar Int` eliminates variable name conflicts
- **Type Expressions**: Types can appear at the expression level
- **Comprehensive Operations**: Supports all language constructs
- **Dependent Types**: `DBExprDepFun` for dependent function types

### De Bruijn Types

```haskell
data DBType
  = DBTNat                      -- Natural number type
  | DBTBool                     -- Boolean type
  | DBTU                        -- Universe type
  | DBTFun DBType DBType        -- Function type
  | DBTDepFun DBType DBType     -- Dependent function type
  | DBTVar Int                  -- Type variable (De Bruijn index)
  | DBTTop                      -- Top type
  | DBTBot                      -- Bot type
  | DBTPair DBType DBType       -- Pair type
  | DBTVec DBType DBExp         -- Vector type [Vector A n]
  deriving (Show, Eq)
```

**Purpose**: Type representations with De Bruijn indexing

**Notable Features**:

- **DBTVar**: Type variables with De Bruijn indices
- **DBTDepFun**: Dependent function types
- **DBTVec**: Dependent vector types with length expressions

### De Bruijn Statements

```haskell
data DBStmt
  = DBSLet DBExp                -- Value binding
  | DBSLetAnn DBType DBExp      -- Type-annotated value binding
  | DBSFun Ident DBType DBExp   -- Function definition
  deriving (Show, Eq)
```

**Purpose**: Statement representations without variable names

## 3. Environment Types (`/src/DBEnv.hs`)

### De Bruijn Environment

```haskell
type DBEnv a = [a]              -- List-based environment for De Bruijn indices
```

**Purpose**: Maps De Bruijn indices to values/types

**Operations**:

- Index-based lookup using list position
- Extension by prepending to list
- Natural correspondence with De Bruijn indexing

### Function Environment

```haskell
type FunEnv a = Map.CustomMap Ident a
```

**Purpose**: Maps function names to their definitions

**Usage**: Preserves named function lookups alongside De Bruijn indexing

## 4. Custom Data Structures (`/src/CustomMap.hs`)

### Custom Map

```haskell
type CustomMap k v = [(k, v)]   -- Association list implementation
```

**Purpose**: Simple key-value mapping for function environments

**Benefits**:

- Simple implementation
- Functional interface
- Sufficient for small function sets

## 5. Logging Infrastructure (`/src/Logger.hs`)

### Log Levels

```haskell
data LogLevel = DEBUG | INFO | WARN | ERROR
  deriving (Show, Eq, Ord)
```

**Purpose**: Hierarchical log filtering

**Ordering**: `DEBUG < INFO < WARN < ERROR` for level-based filtering

### Log Messages

```haskell
data LogMessage = LogMessage
  { logLevel :: LogLevel          -- Message severity
  , logTimestamp :: UTCTime       -- When message was created
  , logComponent :: Text          -- Which component logged it
  , logText :: Text              -- The actual message
  }
  deriving (Show)
```

**Purpose**: Structured log entries with metadata

**Fields**:

- **logLevel**: For filtering by severity
- **logTimestamp**: For chronological ordering
- **logComponent**: For component-based filtering
- **logText**: The actual log message content

### Logger State

```haskell
data Logger = Logger
  { logQueue :: TQueue LogMessage      -- Message buffer
  , loggerLevel :: TVar LogLevel       -- Minimum log level
  , logOutput :: TVar Handle           -- Output destination
  , loggerAsync :: TVar (Maybe (Async ()))  -- Background worker
  }
```

**Purpose**: Concurrent logger with STM-based state management

**STM Components**:

- **TQueue**: Thread-safe message queue
- **TVar**: Atomic mutable references for configuration
- **Async**: Background worker thread management

## 6. Enhanced Evaluator Types (`/src/LoggingEvaluator.hs`)

### Logging Evaluator

```haskell
newtype LoggingEvaluator = LoggingEvaluator {logger :: Logger}
```

**Purpose**: Newtype wrapper adding logging capabilities to evaluation

**Pattern**: Composition over inheritance for adding logging effects

## 7. Type Relationships and Conversions

### Surface to De Bruijn Conversion

- `toDB :: Context -> Exp -> DBExp`
- `toDBType :: Context -> Type -> DBType`
- `toDBStmt :: Context -> Stmt -> DBStmt`

### Context Management

```haskell
type Context = [Ident]          -- Variable name context for conversion
```

**Purpose**: Tracks variable names during De Bruijn conversion

## Summary

The type system demonstrates several key design principles:

1. **Separation of Concerns**: Surface syntax vs. internal representation
2. **Type Safety**: Strong typing throughout evaluation pipeline
3. **Concurrent Safety**: STM types for thread-safe logging
4. **Dependent Types**: Support for types depending on values
5. **Functional Purity**: Immutable data structures with controlled effects

This comprehensive type system enables a robust, extensible functional language interpreter with advanced features like dependent types, concurrent logging, and De Bruijn indexing for variable management.
