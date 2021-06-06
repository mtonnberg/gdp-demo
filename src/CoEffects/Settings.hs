{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CoEffects.Settings
  ( getSettings,
    ApplicationSettings (..),
  )
where

import Control.Exception (SomeException, try)
import Data.AppSettings
import qualified Data.Text as T
import Effects.Logging
  ( Environment (Development),
    LogSettings (..),
  )
import System.Environment (lookupEnv)

data ApplicationSettings = ApplicationSettings
  { port :: Int,
    logSettings :: LogSettings
  }

-- | default TCP port
portSetting :: Setting Int
portSetting = Setting "port" 8484

-- | default behaviour for logging to console
logToConsoleSetting :: Setting Bool
logToConsoleSetting = Setting "logToConsole" True

-- | default logging server URL
grayLogServerUrlSetting :: Setting T.Text
grayLogServerUrlSetting = Setting "graylogserver" "http://localhost"

grayLogPropApplicationSetting :: Setting T.Text
grayLogPropApplicationSetting =
  Setting "graylogpropApplication" "gdp-demo"

grayLogPropEnvironmentSetting :: Setting Environment
grayLogPropEnvironmentSetting = Setting "graylogpropEnvironment" Development

getSettings :: FilePath -> IO ApplicationSettings
getSettings = importSettings

importSettings ::
  -- | Path of "app.config", relative to the Cabal file
  FilePath ->
  IO ApplicationSettings
importSettings path = do
  mPortFromEnvironment <- lookupEnv "APP_PORT"
  readResult <- try $ readSettings (Path path)
  case readResult of
    Right (_, GetSetting getSetting) ->
      return
        ApplicationSettings
          { port = maybe (getSetting portSetting) read mPortFromEnvironment,
            logSettings =
              LogSettings
                { serverUrl = getSetting grayLogServerUrlSetting,
                  logToConsole = getSetting logToConsoleSetting,
                  application = getSetting grayLogPropApplicationSetting,
                  environment = getSetting grayLogPropEnvironmentSetting
                }
          }
    Left (x :: SomeException) ->
      error ("Error reading the config file! " ++ show x)
