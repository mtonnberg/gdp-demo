{-# LANGUAGE BangPatterns #-}
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
    proveHasAMaximumLengthOf,
    proveHasAccessToHabitat,
    proveInHabitat,
    proveIsAValidatedAnimal,
    proveIsNonEmpty,
  )
import DomainIndependent.GDPExtras (Named, SuchThat, SuchThatIt)
import Effects.Logging (LogSettings, logError, logWarning)
import GDP (exorcise, name, unname, (...))
import Servant (ServerError, err401, err500)

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
        let validatedAnimals = allAnimalsForHabitat (exorcise habitat) animals
         in name
              validatedAnimals
              ( \namedResultList ->
                  let mLengthProof = proveHasAMaximumLengthOf namedResultList (exorcise pagesize)
                   in case mLengthProof of
                        Nothing -> do
                          logError logger "Result is greater than pageSize!"
                          return $ Left err500
                        Just r -> return $ Right (unname (namedResultList ... r) ... accessProof)
              )
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
