module Main where

import Control.Exception (bracket)
import Logger
import LoggingEvaluator
import System.Environment (getArgs)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stderr,
    stdout,
  )

-- | Evaluate a program with logging enabled
eval :: String -> IO ()
eval program = do
  bracket
    (newLoggingEvaluator INFO stderr) -- Log to stderr at INFO level by default
    shutdownLoggingEvaluator
    $ \evaluator -> do
      result <- runWithLogging evaluator program
      putStrLn $ case result of
        Left err -> err
        Right val -> show val

main :: IO ()
main = do
  -- Read command-line arguments
  args <- getArgs
  case args of
    -- If no argument is provided, use standard input with Haskeline
    [] -> do
      hSetBuffering stdout NoBuffering
      putStrLn "AFP Language Interpreter with Concurrent Logging"
      putStrLn "Logging to stderr at INFO level"
      putStrLn "=============================================="
      loop
    -- If a file is provided, read the file and interpret it
    (fileName : _) -> do
      program <- readFile fileName
      eval program
  where
    loop :: IO ()
    loop = do
      putStr "Enter an expression (:q to quit): "
      input <- getLine
      case input of
        ":q" -> putStrLn "Goodbye!"
        prog -> do
          eval prog
          loop
