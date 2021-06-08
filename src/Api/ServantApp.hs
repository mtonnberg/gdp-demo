{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.ServantApp
  ( app,
    mkApp,
    runApp,
    getSettings,
  )
where

import Api.ApiDeclaration (api)
import Api.ApiImplementation (server)
import Api.Auth
  ( LoggedInUser,
    UserName,
    authHandler,
  )
import CoEffects.Settings
  ( ApplicationSettings (..),
    getSettings,
    logSettings,
    port,
  )
import Data.Foldable (traverse_)
import Servant.GDP (Named)
import Effects.Logging (LogSettings, logException, logRequest)
import Network.Wai (Application, Request)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setOnException,
    setPort,
  )
import Network.Wai.Middleware.Cors
  ( cors,
    corsMaxAge,
    corsMethods,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant (Context (..), serveWithContext)
import Servant.Server.Experimental.Auth (AuthHandler)

app ::
  LogSettings ->
  Application
app logger = do
  cors (const $ Just policy) $
    provideOptions
      api
      (serveWithContext api ctx (server logger))
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "DELETE", "POST"],
          corsRequestHeaders = ["content-type", "Access-Control-Token"],
          corsMaxAge = Just (1 * 60 * 60 * 24 * 14)
        }
    ctx :: Context (AuthHandler Request (LoggedInUser `Named` UserName) ': '[])
    ctx = authHandler :. EmptyContext

mkApp ::
  FilePath ->
  IO Application
mkApp configPath = do
  settings <- getSettings configPath
  let logger = logSettings settings
  return $ app logger

runApp :: FilePath -> IO ()
runApp configPath = do
  appSettings <- getSettings configPath
  printSettings appSettings
  let settings =
        setPort (port appSettings) $
          setOnException (logException $ logSettings appSettings) $
            setLogger (logRequest $ logSettings appSettings) defaultSettings
   in runSettings settings
        =<< mkApp configPath

-- | Log some configuration to stdout
printSettings :: ApplicationSettings -> IO ()
printSettings (ApplicationSettings p _) = do
  putStrLn ""
  traverse_
    Prelude.putStrLn
    ["server listening on port " <> show p]
  putStrLn ""
