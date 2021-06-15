{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.ApiDeclaration where

import Api.Auth
import Data.Proxy (Proxy (Proxy))
import Domain.DomainProofs (HasAMaximumLengthOf, HasAccessToHabitat, IsAValidatedAnimal, IsNonEmpty, IsPositive)
import Servant (Get, JSON, (:>))
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.GDP (ApiName0, ApiName1, CaptureNamed)
import DomainIndependent.GDPHumanReadable (Named, SuchThat, SuchThatIt)

type GetAnimalsForHabitat user habitat pagesize =
  AuthProtect "normalUser"
    :> "habitats"
    :> CaptureNamed (String `Named` habitat `SuchThat` IsNonEmpty habitat)
    :> "animals"
    :> CaptureNamed (Int `Named` pagesize `SuchThat` IsPositive pagesize)
    :> Get
         '[JSON]
         ( ( [String `SuchThatIt` IsAValidatedAnimal habitat] `SuchThatIt` HasAMaximumLengthOf pagesize
           )
             `SuchThat` (user `HasAccessToHabitat` habitat)
         )

type Api =
  GetAnimalsForHabitat UserName ApiName0 ApiName1

api :: Proxy Api
api = Proxy
