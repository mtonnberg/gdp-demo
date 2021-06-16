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
    IsTrimmed,
  )
import DomainIndependent.GDPAlternativeNaming (Named, SuchThat, That)
import Effects.Logging (LogSettings)
import GDP (type (&&))
import Servant (Handler, Server, throwError)

server ::
  LogSettings ->
  Server Api
server logger = do
  getAnimalsForHabitatH
  where
    getAnimalsForHabitatH ::
      LoggedInUser `Named` user ->
      String `Named` habitat `SuchThat` (IsNonEmpty habitat && IsTrimmed habitat) ->
      Int `Named` pagesize `SuchThat` IsPositive pagesize ->
      Handler
        ( ( [String `That` IsAValidatedAnimal habitat]
              `That` HasAMaximumLengthOf pagesize
          )
            `SuchThat` HasAccessToHabitat user habitat
        )
    getAnimalsForHabitatH user habitat pagesize =
      do
        r <- liftIO $ getAnimalsForHabitat logger user habitat pagesize
        either throwError return r
