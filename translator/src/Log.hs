module Log
  ( logDebug,
    logInfo,
    logError,
  )
where

import Cli
  ( quiet,
    verbose,
  )
import Colog
  ( LogAction (LogAction),
    Severity (Debug, Error, Info),
    filterBySeverity,
    showSeverity,
    (<&),
  )
import Data.Text
  ( Text,
    pack,
  )
import Data.Text.IO
  ( putStrLn,
  )
import Prelude hiding
  ( putStrLn,
  )

data LogMessage = LogMessage
  { logSeverity :: Severity,
    _logMsg :: Text
  }

fmtLogMessage :: LogMessage -> Text
fmtLogMessage (LogMessage severity message) = showSeverity severity <> message

logger :: IO (LogAction IO LogMessage)
logger = do
  let action = LogAction (putStrLn . fmtLogMessage)
  quiet' <- quiet
  if quiet'
    then return $ filterBySeverity Error logSeverity action
    else do
      verbose' <- verbose
      if verbose'
        then return $ filterBySeverity Debug logSeverity action
        else return $ filterBySeverity Info logSeverity action

logDebug :: String -> IO ()
logDebug msg = logger >>= (<& LogMessage Debug (pack msg))

logInfo :: String -> IO ()
logInfo msg = logger >>= (<& LogMessage Info (pack msg))

logError :: String -> IO ()
logError msg = logger >>= (<& LogMessage Error (pack msg))
