{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.ApiDeclaration where

import Api.Auth
import Data.Proxy (Proxy (Proxy))
import Domain.DomainProofs (HasAMaximumLengthOf, HasAccessToHabitat, IsAValidatedAnimal, IsNonEmpty, IsPositive, IsTrimmed)
import DomainIndependent.GDPAlternativeNaming (Named, SuchThat, That)
import GDP (type (&&))
import Servant (Get, JSON, (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.GDP (ApiName0, ApiName1, CaptureNamed)

type GetAnimalsForHabitat user habitat pagesize =
  AuthProtect "normalUser"
    :> "habitats"
    :> CaptureNamed (String `Named` habitat `SuchThat` IsNonEmpty habitat && IsTrimmed habitat)
    :> "animals"
    :> CaptureNamed (Int `Named` pagesize `SuchThat` IsPositive pagesize)
    :> Get
         '[JSON]
         ( ( [String `That` IsAValidatedAnimal habitat] `That` HasAMaximumLengthOf pagesize
           )
             `SuchThat` user `HasAccessToHabitat` habitat
         )

type Api =
  GetAnimalsForHabitat UserName ApiName0 ApiName1

api :: Proxy Api
api = Proxy
