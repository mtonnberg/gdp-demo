{-# LANGUAGE OverloadedStrings #-}

module Effects.Logging.LogSettings
  ( Environment (..),
    LogSettings (..),
    envToText,
  )
where

import qualified Data.Text as T

data LogSettings = LogSettings
  { serverUrl :: T.Text,
    logToConsole :: Bool,
    application :: T.Text,
    environment :: Environment
  }

data Environment
  = Development
  | Production
  | Staging

instance Read Environment where
  readsPrec _ "Production" = [(Production, "")]
  readsPrec _ "Staging" = [(Staging, "")]
  readsPrec _ _ = [(Development, "")]

envToText :: Environment -> T.Text
envToText Development = "development"
envToText Production = "production"
envToText Staging = "staging"
