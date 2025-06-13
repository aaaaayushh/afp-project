{-# LANGUAGE OverloadedStrings #-}

module LoggingEvaluator where

import Control.Monad.IO.Class
import DBEnv
  ( DBEnv,
    FunEnv,
    emptyDB,
    emptyFun,
  )
import Data.Text qualified as T
import Evaluator (Evaluator, Result, throw)
import Evaluator qualified as E
import Interp.DBProg (interp)
import Lang.Abs
  ( Program,
    Type,
  )
import Lang.Par
  ( myLexer,
    pProgram,
  )
import Logger
import System.IO
import TypeCheck.DBProg (infer)
import Value (Value)

-- | A logging-enabled evaluator that maintains all existing functionality
-- while adding logging capabilities
newtype LoggingEvaluator = LoggingEvaluator {logger :: Logger}

-- | Create a new logging evaluator with specified log level
newLoggingEvaluator :: LogLevel -> Handle -> IO LoggingEvaluator
newLoggingEvaluator level handle = do
  lgr <- newLogger level handle
  startLogger lgr
  return $ LoggingEvaluator lgr

-- | Clean shutdown of the logging evaluator
shutdownLoggingEvaluator :: LoggingEvaluator -> IO ()
shutdownLoggingEvaluator eval = stopLogger (logger eval)

-- | Enhanced version of the original evaluate function with logging
evaluateWithLogging :: LoggingEvaluator -> (Evaluator a b, String) -> String -> IO (Result a)
evaluateWithLogging eval (evaluator, errDesc) input = do
  let lgr = logger eval

  -- Log the start of evaluation
  logInfo lgr "Evaluator" $ "Starting evaluation: " <> T.pack errDesc
  logDebug lgr "Evaluator" $
    "Input program: "
      <> T.pack (take 100 input)
      <> (if length input > 100 then "..." else "")

  -- Parse the program
  parseResult <- case pProgram (myLexer input) of
    Left parseErr -> do
      let errMsg = "Parse error: " ++ parseErr
      logError lgr "Parser" $ T.pack errMsg
      return $ Left errMsg
    Right prog -> do
      logInfo lgr "Parser" "Successfully parsed program"
      return $ Right prog

  case parseResult of
    Left err -> return $ Left err
    Right prog -> do
      -- Log environment initialization
      logDebug lgr "Evaluator" "Initializing evaluation environment"

      -- Perform evaluation
      logInfo lgr "Evaluator" $ "Running " <> T.pack errDesc
      case evaluator prog (emptyDB, emptyFun) of
        Left err -> do
          let fullErr = errDesc ++ " error: " ++ err
          logError lgr (T.pack errDesc) $ T.pack err
          return $ Left fullErr
        Right result -> do
          logInfo lgr (T.pack errDesc) "Evaluation completed successfully"
          return $ Right result

-- | Enhanced version of the original infertype function
infertypeWithLogging :: LoggingEvaluator -> String -> IO (Result Type)
infertypeWithLogging eval input = do
  let lgr = logger eval
  logInfo lgr "TypeChecker" "Starting type inference"
  result <- evaluateWithLogging eval (infer, "Type") input
  case result of
    Left err -> logError lgr "TypeChecker" $ "Type inference failed: " <> T.pack err
    Right _ -> logInfo lgr "TypeChecker" "Type inference succeeded"
  return result

-- | Enhanced version of the original run function
runWithLogging :: LoggingEvaluator -> String -> IO (Result Value)
runWithLogging eval input = do
  let lgr = logger eval
  logInfo lgr "Runtime" "Starting program execution"

  -- First do type checking
  typeResult <- infertypeWithLogging eval input
  case typeResult of
    Left err -> do
      logError lgr "Runtime" $ "Type checking failed, aborting execution: " <> T.pack err
      return $ Left err
    Right _ -> do
      logInfo lgr "Runtime" "Type checking passed, proceeding with evaluation"
      result <- evaluateWithLogging eval (interp, "Runtime") input
      case result of
        Left err -> logError lgr "Runtime" $ "Runtime error: " <> T.pack err
        Right _ -> logInfo lgr "Runtime" "Program execution completed successfully"
      return result