{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.ApiImplementation
  ( server,
  )
where

import Api.ApiDeclaration (Api)
import Api.Auth
  ( LoggedInUser,
  )
import Control.Monad.IO.Class (liftIO)
import Domain.Animals (getAnimalsForHabitat)
import Domain.DomainProofs
  ( HasAMaximumLengthOf,
    HasAccessToHabitat,
    IsAValidatedAnimal,
    IsNonEmpty,
    IsPositive,
  )
import DomainIndependent.GDPExtras (Named, SuchThat, SuchThatIt)
import Effects.Logging (LogSettings)
import Servant (Handler, Server, throwError)

server ::
  LogSettings ->
  Server Api
server logger = do
  getAnimalsForHabitatH
  where
    getAnimalsForHabitatH ::
      LoggedInUser `Named` user ->
      String `Named` habitat `SuchThat` IsNonEmpty habitat ->
      Int `Named` pagesize `SuchThat` IsPositive pagesize ->
      Handler
        ( ( [String `SuchThatIt` IsAValidatedAnimal habitat]
              `SuchThatIt` HasAMaximumLengthOf pagesize
          )
            `SuchThat` HasAccessToHabitat user habitat
        )
    getAnimalsForHabitatH user habitat pagesize =
      do
        r <- liftIO $ getAnimalsForHabitat logger user habitat pagesize
        either throwError return r
