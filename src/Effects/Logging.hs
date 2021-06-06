{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Effects.Logging
  ( logCritical,
    logError,
    logException,
    logWarning,
    logInfo,
    logRequest,
    module Effects.Logging.LogSettings,
    Log.runLoggingT,
    CCLogger,
    standardMonadicLogger,
  )
where

import Control.Exception (SomeException)
import Control.Monad.Logger
  ( Loc,
    LogLevel (..),
    LogSource,
    LogStr,
  )
import qualified Control.Monad.Logger as Log
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Effects.Logging.Graylog.GelfMessage (toGelfMessage)
import Effects.Logging.Graylog.GraylogServer
import Effects.Logging.Graylog.MonadicLogger (graylogLogging)
import Effects.Logging.Graylog.Sending (openConnectionAndSendMessage)
import Effects.Logging.LogSettings
  ( Environment (..),
    LogSettings (..),
  )
import Effects.Logging.Stdout.Stdout
import Network.HTTP.Types (Status (..))
import qualified Network.Wai as Wai
import System.IO (stdout)
import Prelude hiding (log)

type CCLogger = (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

logCritical :: LogSettings -> T.Text -> IO ()
logCritical = log LevelError "application" []

logError :: LogSettings -> T.Text -> IO ()
logError = log LevelError "application" []

logWarning :: LogSettings -> T.Text -> IO ()
logWarning = log LevelWarn "application" []

logInfo :: LogSettings -> T.Text -> IO ()
logInfo = log LevelInfo "application" []

logException :: LogSettings -> Maybe Wai.Request -> SomeException -> IO ()
logException settings request exception = do
  additionalFields <- getAdditionalFields request
  log (errorLevel message) "exception" additionalFields settings message
  where
    getAdditionalFields = maybe (return []) getRequestFields
    message = T.pack $ show exception
    errorLevel "threadWait: invalid argument (Bad file descriptor)" = LevelDebug
    errorLevel _ = LevelError

logRequest :: LogSettings -> Wai.Request -> Status -> Maybe Integer -> IO ()
logRequest settings request status _ = do
  requestFields <- getRequestFields request
  log LevelInfo source (requestFields ++ statusFields) settings msg
  where
    source = "servant-request"
    statusFields =
      [ ("statusCode", T.pack $ show $ statusCode status),
        ("statusMessage", T.decodeUtf8 $ statusMessage status)
      ]
    msg = T.decodeUtf8 $ Wai.rawPathInfo request

getRequestFields :: Wai.Request -> IO [(T.Text, T.Text)]
getRequestFields request = do
  reqBody <- Wai.getRequestBodyChunk request
  return
    [ ("method", T.decodeUtf8 $ Wai.requestMethod request),
      ("httpVersion", T.pack $ show $ Wai.httpVersion request),
      ("rawPathInfo", T.decodeUtf8 $ Wai.rawPathInfo request),
      ("rawQueryString", T.decodeUtf8 $ Wai.rawQueryString request),
      ("isSecure", T.pack $ show $ Wai.isSecure request),
      ("remoteHost", T.pack $ show $ Wai.remoteHost request),
      ("pathInfo", T.intercalate "/" $ Wai.pathInfo request),
      ("queryString", T.pack $ show $ Wai.queryString request),
      ("requestBody", T.decodeUtf8 reqBody),
      ("requestBodyLength", T.pack $ show $ Wai.requestBodyLength request),
      ("requestHeaderHost", utf8OrEmpty $ Wai.requestHeaderHost request),
      ("requestHeaderRange", utf8OrEmpty $ Wai.requestHeaderRange request)
    ]
  where
    utf8OrEmpty = T.decodeUtf8 . fromMaybe ""

log ::
  LogLevel -> T.Text -> [(T.Text, T.Text)] -> LogSettings -> T.Text -> IO ()
log l source af s msg =
  openConnectionAndSendMessage (serverUrl s) $
    toGelfMessage l "carbondata" source af (environment s) msg

standardMonadicLogger ::
  LogSettings -> IO (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
standardMonadicLogger logSettings = do
  graylogServer <- openGraylog (T.unpack . serverUrl $ logSettings) "12201" 1024
  return $
    case graylogServer of
      Left _ -> defaultOutput stdout
      Right gray -> graylogLogging gray logSettings "carbondata"
