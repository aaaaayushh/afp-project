# Module Structure Documentation

## Overview

This Haskell codebase implements a functional programming language interpreter with dependent types, De Bruijn indexing, and advanced logging capabilities. The project is organized into three main directories: `/src` (source code), `/grammar` (language grammar), and `/test` (test suite).

## Core Module Hierarchy

### 1. Language Definition and Parsing (`/grammar`)

- **Lang.cf**: BNFC grammar definition file that specifies the syntax of the functional language
  - Defines types (nat, bool, U, Top, Bot, pairs, vectors, functions)
  - Expression syntax with proper precedence rules
  - Statement syntax for value bindings and function definitions
  - Supports dependent types with `(x : Type) -> Type` syntax
  - Generated modules: `Lang.Abs`, `Lang.Par`, `Lang.Lex` (abstract syntax, parser, lexer)

### 2. Core Data Structures (`/src`)

#### Value System

- **Value.hs**: Defines the runtime value representation
  - Core `Value` data type with variants for naturals, booleans, lambdas, types, pairs, vectors
  - `Nat` representation using (Zero, Suc)
  - `Closure` type for function values with De Bruijn and named variants
  - Helper functions for value construction

#### De Bruijn System

- **DeBruijn.hs**: Comprehensive De Bruijn indexing implementation
  - `DBExp` type for expressions with De Bruijn indices
  - `DBType` type for types with variable support
  - `DBStmt` type for statements in De Bruijn form
  - Conversion functions: `toDB`, `toDBType`, `toDBStmt`
  - Substitution and shifting operations with lens-based traversals
  - Supports all language constructs including dependent types and vectors

#### Environment Management

- **DBEnv.hs**: Environment abstractions for evaluation

  - `DBEnv` type alias for list-based variable environments
  - `FunEnv` type for function name environments using CustomMap
  - Operations: `lookupDB`, `extendDB`, `lookupFun`, `bindFun`

- **CustomMap.hs**: Simple association list-based map implementation
  - `CustomMap` type as key-value pairs
  - Basic operations: `empty`, `insert`, `lookup`
  - Used for function name resolution

### 3. Evaluation System (`/src`)

#### Core Evaluation

- **Evaluator.hs**: Base evaluation types and error handling
  - `Result` type alias for `Either String a`
  - `Evaluator` type for program evaluation functions
  - `evaluate` function for parsing and evaluation coordination

#### Interpretation Modules (`/src/Interp/`)

- **DBProg.hs**: Program-level interpreter entry point
- **DBStmt.hs**: Statement evaluation (let bindings, function definitions)
- **DBExpr.hs**: Expression evaluation with comprehensive pattern matching
  - Arithmetic operations on natural numbers
  - Boolean logic and comparisons
  - Lambda expressions and function application
  - Type expressions and dependent type support
  - Vector operations (head, tail, cons, append)
  - Pair operations (fst, snd, pair construction)

### 4. Type System (`/src/TypeCheck/`)

- **DBProg.hs**: Program-level type checking entry point
- **DBStmt.hs**: Statement type checking
- **DBExpr.hs**: Expression type inference and checking
  - Bidirectional type checking
  - Support for dependent types and universe levels
  - Type inference for all language constructs

### 5. Logging System (`/src`)

#### Core Logging Infrastructure

- **Logger.hs**: Concurrent logging system using STM
  - `LogLevel` enumeration (DEBUG, INFO, WARN, ERROR)
  - `LogMessage` type with timestamp and component information
  - `Logger` type with STM-based message queue and configuration
  - Asynchronous logging worker with configurable output handles
  - Thread-safe logging operations

#### Enhanced Evaluation

- **LoggingEvaluator.hs**: Logging-enhanced evaluation wrapper
  - `LoggingEvaluator` newtype wrapping Logger
  - Enhanced versions of core functions: `evaluateWithLogging`, `runWithLogging`
  - Comprehensive logging at all evaluation stages
  - Integration with existing evaluation pipeline

### 6. Test Suite (`/test`)

#### Test Organization

- **Main.hs**: Test suite orchestration
- **Phase1Tests.hs**: Basic language feature tests
- **Phase2Tests.hs**: Advanced type system tests
- **Phase3Tests.hs**: Vector and dependent type tests
- **DeBruijnTests.hs**: De Bruijn conversion and manipulation tests
- **CustomMapTests.hs**: Data structure tests
- **InterpTests.hs**: Interpreter functionality tests
- **TypeCheckTests.hs**: Type system tests
- **BogusTests.hs**: Error case and edge case tests

## Module Dependencies

The dependency structure follows a clear hierarchy:

1. **Foundation**: `CustomMap` → `Lang.Abs` (generated)
2. **Core Types**: `Value`, `DeBruijn` → Foundation
3. **Environment**: `DBEnv` → Core Types
4. **Evaluation**: `Evaluator` → Environment, `Interp/*` → Evaluation
5. **Type Checking**: `TypeCheck/*` → Evaluation
6. **Logging**: `Logger` → STM libraries, `LoggingEvaluator` → Logger + Evaluation
7. **Tests**: All test modules → Corresponding source modules

This structure ensures clean separation of concerns and enables modular development and testing of each component.
