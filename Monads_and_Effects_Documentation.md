# Monads and Effects Documentation

## Overview

This codebase employs several monadic patterns and effect systems to handle evaluation, error management, and logging in a functional programming language interpreter. The design emphasizes composability, error handling, and concurrent logging.

## 1. Core Monadic Abstractions

### Result Monad (`/src/Evaluator.hs`)

```haskell
type Result a = Either String a
```

**Purpose**: Provides error handling throughout the evaluation pipeline

**Key Operations**:

- `throw :: String -> Result a` - Lifts error messages into the Result context
- Standard `Either` operations for error propagation and handling
- Monadic bind (`>>=`) for error-aware computation chaining

**Usage Pattern**:

- All evaluation functions return `Result Value` or `Result Type`
- Errors automatically propagate through monadic composition
- Enables clean separation of success and failure paths

**Example**:

```haskell
interp (DBAdd e1 e2) env = do
  v1 <- interp e1 env  -- Automatically propagates errors
  v2 <- interp e2 env
  case (v1, v2) of
    (VNat n1, VNat n2) -> return $ VNat (addNat n1 n2)
    _ -> throw "Arithmetic can only be performed on natural numbers"
```

### IO Monad for System Effects

**Parsing and Input/Output**:
The codebase uses IO for:

- File parsing with BNFC-generated parsers
- Console input/output
- Logging operations
- Concurrent thread management

**Key Functions**:

- `evaluate :: (Evaluator a b, String) -> String -> Result a`
- `pProgram :: [Token] -> Either String Program`

## 2. Software Transactional Memory (STM)

### STM Monad in Logger (`/src/Logger.hs`)

```haskell
data Logger = Logger
  { logQueue :: TQueue LogMessage,
    loggerLevel :: TVar LogLevel,
    logOutput :: TVar Handle,
    loggerAsync :: TVar (Maybe (Async ()))
  }
```

**Purpose**: Provides thread-safe, concurrent logging with atomic operations

**STM Operations Used**:

- `TQueue` for message queuing with atomic enqueue/dequeue
- `TVar` for mutable state management (log level, output handle, async worker)
- `atomically` for composing STM actions into atomic transactions

**Concurrency Model**:

- **Producer**: Main evaluation thread writes log messages to queue
- **Consumer**: Background worker thread reads from queue and writes to output
- **Atomic State**: Log level and output configuration can be changed atomically

**Key STM Functions**:

```haskell
-- Atomic message queuing
logMessage :: Logger -> LogLevel -> Text -> Text -> IO ()
logMessage logger level component message = do
  timestamp <- getCurrentTime
  let msg = LogMessage level timestamp component message
  atomically $ writeTQueue (logQueue logger) msg

-- Atomic configuration changes
setLogLevel :: Logger -> LogLevel -> IO ()
setLogLevel logger newLevel =
  atomically $ writeTVar (loggerLevel logger) newLevel
```

## 3. Concurrent Effects with Async

### Asynchronous Logging Worker

```haskell
-- Background worker management
startLogger :: Logger -> IO ()
startLogger logger = do
  asyncAction <- async (loggerWorker logger)
  atomically $ writeTVar (loggerAsync logger) (Just asyncAction)

loggerWorker :: Logger -> IO ()
loggerWorker logger = forever $ do
  msg <- atomically $ readTQueue (logQueue logger)
  -- Process and output message
```

**Benefits**:

- Non-blocking logging operations
- Graceful shutdown with `cancel`
- Separation of logging from main evaluation thread

## 4. Monad Transformers and Composition

### LoggingEvaluator Enhancement

```haskell
newtype LoggingEvaluator = LoggingEvaluator {logger :: Logger}
```

**Pattern**: Newtype wrapper providing enhanced evaluation with logging effects

**Effect Composition**:

- Combines `IO` effects (logging) with `Result` effects (error handling)
- Maintains original evaluation semantics while adding observability
- Clean separation between evaluation logic and logging concerns

**Enhanced Operations**:

```haskell
evaluateWithLogging :: LoggingEvaluator -> (Evaluator a b, String) -> String -> IO (Result a)
```

This function composes multiple effects:

1. **IO** for logging operations
2. **Result** for error handling
3. **STM** for thread-safe logging
4. **Async** for concurrent logging

## 5. Error Handling Patterns

### Hierarchical Error Propagation

The codebase uses a consistent error handling strategy:

**At Expression Level**:

```haskell
interp :: DBExp -> (DBEnv Value, FunEnv Closure) -> Result Value
```

**At Statement Level**:

```haskell
interpStmt :: DBStmt -> (DBEnv Value, FunEnv Closure) -> Result (DBEnv Value, FunEnv Closure)
```

**At Program Level**:

```haskell
interp :: Program -> (DBEnv Value, FunEnv Closure) -> Result Value
```

### Error Context Enhancement

```haskell
evaluateWithLogging eval (evaluator, errDesc) input = do
  -- ... parsing and setup ...
  case evaluator prog (emptyDB, emptyFun) of
    Left err -> do
      let fullErr = errDesc ++ " error: " ++ err
      logError lgr (T.pack errDesc) $ T.pack err
      return $ Left fullErr
    Right result -> do
      logInfo lgr (T.pack errDesc) "Evaluation completed successfully"
      return $ Right result
```

**Benefits**:

- Contextual error messages
- Automatic error logging
- Structured error reporting

## Summary

The codebase demonstrates sophisticated use of Haskell's effect system:

- **Result monad** for clean error handling
- **STM monad** for concurrent logging
- **IO monad** for system interactions
- **Async effects** for concurrent operations

This combination provides a robust, scalable foundation for a functional language interpreter with comprehensive logging and error handling capabilities.
