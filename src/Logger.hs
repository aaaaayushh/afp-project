{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time
import System.IO

-- | Log levels for filtering messages
data LogLevel = DEBUG | INFO | WARN | ERROR
  deriving (Show, Eq, Ord)

-- | A log message with metadata
data LogMessage = LogMessage
  { logLevel :: LogLevel,
    logTimestamp :: UTCTime,
    logComponent :: Text,
    logText :: Text
  }
  deriving (Show)

-- | The logger state using STM
data Logger = Logger
  { logQueue :: TQueue LogMessage,
    loggerLevel :: TVar LogLevel,
    logOutput :: TVar Handle,
    loggerAsync :: TVar (Maybe (Async ()))
  }

-- | Create a new logger with specified minimum log level
newLogger :: LogLevel -> Handle -> IO Logger
newLogger minLevel handle = atomically $ do
  queue <- newTQueue
  level <- newTVar minLevel
  output <- newTVar handle
  asyncVar <- newTVar Nothing
  return $ Logger queue level output asyncVar

-- | Start the logging background thread
startLogger :: Logger -> IO ()
startLogger logger = do
  asyncAction <- async (loggerWorker logger)
  atomically $ writeTVar (loggerAsync logger) (Just asyncAction)

-- | Stop the logging background thread gracefully
stopLogger :: Logger -> IO ()
stopLogger logger = do
  maybeAsync <- readTVarIO (loggerAsync logger)
  case maybeAsync of
    Just asyncAction -> do
      cancel asyncAction
      atomically $ writeTVar (loggerAsync logger) Nothing
    Nothing -> return ()

-- | The background worker that processes log messages
loggerWorker :: Logger -> IO ()
loggerWorker logger = forever $ do
  msg <- atomically $ readTQueue (logQueue logger)
  minLevel <- readTVarIO (loggerLevel logger)
  handle <- readTVarIO (logOutput logger)

  when (logLevel msg >= minLevel) $ do
    let formattedMsg = formatLogMessage msg
    TIO.hPutStrLn handle formattedMsg
    hFlush handle

-- | Format a log message for output
formatLogMessage :: LogMessage -> Text
formatLogMessage (LogMessage level timestamp component text) =
  T.concat
    [ "[",
      T.pack (show timestamp),
      "] ",
      "[",
      T.pack (show level),
      "] ",
      "[",
      component,
      "] ",
      text
    ]

-- | Log a message at a specific level
logMessage :: Logger -> LogLevel -> Text -> Text -> IO ()
logMessage logger level component message = do
  timestamp <- getCurrentTime
  let msg = LogMessage level timestamp component message
  atomically $ writeTQueue (logQueue logger) msg

-- | Convenience functions for different log levels
logDebug, logInfo, logWarn, logError :: Logger -> Text -> Text -> IO ()
logDebug logger = logMessage logger DEBUG
logInfo logger = logMessage logger INFO
logWarn logger = logMessage logger WARN
logError logger = logMessage logger ERROR

-- | Change the minimum log level
setLogLevel :: Logger -> LogLevel -> IO ()
setLogLevel logger newLevel = atomically $ writeTVar (loggerLevel logger) newLevel

-- | Change the output handle
setLogOutput :: Logger -> Handle -> IO ()
setLogOutput logger newHandle = atomically $ writeTVar (logOutput logger) newHandle