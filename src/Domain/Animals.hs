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
import Data.Maybe (catMaybes)
import Domain.DomainProofs
  ( HasAMaximumLengthOf,
    HasAccessToHabitat,
    IsAValidatedAnimal,
    IsNonEmpty,
    IsPositive,
    IsTrimmed,
    proveHasAccessToHabitat,
    proveInHabitat,
    proveIsAValidatedAnimal,
    proveIsNonEmpty,
    takeXElements,
  )
import DomainIndependent.GDPHumanReadable (Named, SuchThat, SuchThatIt, withProof, withoutProof)
import Effects.Logging (LogSettings, logWarning)
import GDP (name, unname, type (&&))
import Servant (ServerError, err401)

getAnimalsForHabitat ::
  LogSettings ->
  LoggedInUser `Named` user ->
  String `Named` habitat `SuchThat` (IsNonEmpty habitat && IsTrimmed habitat) ->
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
    animals <- allAnimals
    case proveHasAccessToHabitat user (withoutProof habitat) of
      Just accessProof ->
        let validatedAnimals =
              takeXElements pagesize $
                allAnimalsForHabitat (withoutProof habitat) animals
         in return $ Right $ validatedAnimals `withProof` accessProof
      Nothing -> do
        logWarning logger "GDP demo test message, wrong user"
        return $ Left err401

allAnimalsForHabitat ::
  forall habitat.
  String `Named` habitat ->
  [String] ->
  [String `SuchThatIt` IsAValidatedAnimal habitat]
allAnimalsForHabitat habitat animals =
  let rawResults :: [Maybe (String `SuchThatIt` IsAValidatedAnimal habitat)]
      rawResults = map (\animal -> name animal (fmap unname . mkValidatedAnimal)) animals
   in catMaybes rawResults
  where
    mkValidatedAnimal ::
      String `Named` animal ->
      Maybe (String `Named` animal `SuchThat` IsAValidatedAnimal habitat animal)
    mkValidatedAnimal namedAnimal =
      case (proveInHabitat namedAnimal habitat, proveIsNonEmpty namedAnimal) of
        (Just habitatProof, Just animalIsNonEmptyProof) ->
          let isValidatedProof =
                proveIsAValidatedAnimal
                  (habitat `withProof` habitatProof)
                  (namedAnimal `withProof` animalIsNonEmptyProof)
           in Just (namedAnimal `withProof` isValidatedProof)
        _ -> Nothing

allAnimals :: IO [String]
allAnimals = do
  -- This could come from database for example
  return ["polarbear", "lion", "seal", "pumba"]
