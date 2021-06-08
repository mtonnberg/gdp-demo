{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Domain.Animals
  ( getAnimalsForHabitat,
  )
where

import Api.Auth (LoggedInUser)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import Domain.DomainProofs
  ( HasAMaximumLengthOf,
    HasAccessToHabitat,
    IsAValidatedAnimal,
    IsNonEmpty,
    IsPositive,
    proveHasAccessToHabitat,
    proveInHabitat,
    proveIsAValidatedAnimal,
    proveIsNonEmpty,
    takeXElements,
  )
import Servant.GDP (Named, SuchThat, SuchThatIt)
import Effects.Logging (LogSettings, logWarning)
import GDP (exorcise, name, unname, (...))
import Servant (ServerError, err401)

getAnimalsForHabitat ::
  LogSettings ->
  LoggedInUser `Named` user ->
  String `Named` habitat `SuchThat` IsNonEmpty habitat ->
  Int `Named` pagesize `SuchThat` IsPositive pagesize ->
  IO
    ( Either
        ServerError
        ( ( [String `SuchThatIt` IsAValidatedAnimal habitat]
              `SuchThatIt` HasAMaximumLengthOf pagesize
          )
            `SuchThat` (user `HasAccessToHabitat` habitat)
        )
    )
getAnimalsForHabitat logger user habitat pagesize =
  do
    animals <- liftIO allAnimals
    case proveHasAccessToHabitat user (exorcise habitat) of
      Just accessProof ->
        let validatedAnimals =
              takeXElements pagesize $
                allAnimalsForHabitat (exorcise habitat) animals
         in return $ Right $ validatedAnimals ... accessProof
      Nothing -> do
        logWarning logger "GDP demo test message, wrong user"
        return $ Left err401

allAnimalsForHabitat :: forall habitat. String `Named` habitat -> [String] -> [String `SuchThatIt` IsAValidatedAnimal habitat]
allAnimalsForHabitat habitat =
  mapMaybe (\a -> name a (fmap unname . mkValidatedAnimal))
  where
    mkValidatedAnimal :: String `Named` animal -> Maybe (String `Named` animal `SuchThat` IsAValidatedAnimal habitat animal)
    mkValidatedAnimal namedAnimal =
      case (proveInHabitat namedAnimal habitat, proveIsNonEmpty namedAnimal) of
        (Just habitatProof, Just animalIsNonEmptyProof) ->
          Just (namedAnimal ... proveIsAValidatedAnimal (habitat ... habitatProof) (namedAnimal ... animalIsNonEmptyProof))
        _ -> Nothing

allAnimals :: IO [String]
allAnimals = do
  return ["polarbear", "lion", "seal", "pumba"]
