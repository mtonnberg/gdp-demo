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
import Domain.DomainProofs (HasAccessToHabitat, IsAValidatedAnimal, HasAMaximumLengthOf, IsNonEmpty, IsPositive)
import DomainIndependent.ApiNamedInput (ApiName0, ApiName1, Capture')
import DomainIndependent.GDPExtras (Named, SuchThat, SuchThatIt)
import Servant (Get, JSON, (:>))
import Servant.API.Experimental.Auth (AuthProtect)

type GetAnimalsForHabitat user habitat pagesize =
  AuthProtect "normalUser"
    :> "habitats"
    :> Capture' (String `Named` habitat `SuchThat` IsNonEmpty habitat)
    :> "animals"
    :> Capture' (Int `Named` pagesize `SuchThat` IsPositive pagesize)
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