module Effects.Logging.Graylog.MonadicLogger where

import Control.Monad.Logger
  ( Loc,
    LogLevel (..),
    LogSource,
    LogStr,
  )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Effects.Logging.Graylog.GelfMessage (toGelfMessage)
import Effects.Logging.Graylog.GraylogServer (GraylogServer)
import Effects.Logging.Graylog.Sending (sendLog)
import Effects.Logging.LogSettings (LogSettings (..))
import System.Log.FastLogger (fromLogStr)
import Prelude hiding (log)

-- Function to hook our Graylog server into the MonadLogger.
graylogLogging ::
  GraylogServer ->
  LogSettings ->
  T.Text ->
  Loc ->
  LogSource ->
  LogLevel ->
  LogStr ->
  IO ()
graylogLogging gray settings host log source level logStr =
  let log' :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
      log' _ src lvl msg = sendLog gray message
        where
          messageText = (T.decodeUtf8 $ fromLogStr msg)
          message =
            toGelfMessage lvl host src [] (environment settings) messageText
   in log' log source level logStr
