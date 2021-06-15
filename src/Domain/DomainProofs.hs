{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Domain.DomainProofs
  ( IsNonEmpty,
    IsPositive,
    BelongsIn,
    IsTrimmed,
    proveHasAccessToHabitat,
    HasAMaximumLengthOf,
    HasAccessToHabitat,
    IsAValidatedAnimal,
    proveInHabitat,
    proveHasAMaximumLengthOf,
    proveIsNonEmpty,
    proveIsAValidatedAnimal,
    takeXElements,
  )
where

import Api.Auth
  ( HabitatAccess (..),
    LoggedInUser (..),
  )
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text
  ( Text,
    stripEnd,
    stripStart,
  )
import DomainIndependent.GDPHumanReadable (Named)
import DomainIndependent.StringConversions (stringToText)
import GDP (Defn, Proof, assert, axiom, the, type (:::), type (?), type (~~))
import Servant.GDP (ProvableInIsolation, proveInIsolation)

newtype IsNonEmpty a = IsNonEmpty Defn

type role IsNonEmpty nominal

newtype IsTrimmed a = IsTrimmed Defn

type role IsTrimmed nominal

newtype IsPositive a = IsPositive Defn

type role IsPositive nominal

newtype BelongsIn habitat animal = BelongsIn Defn

type role BelongsIn nominal nominal

newtype HasAMaximumLengthOf size traversable = HasAMaximumLengthOf Defn

type role HasAMaximumLengthOf nominal nominal

newtype HasAccessToHabitat user habitat = HasAccessToHabitat Defn

type role HasAccessToHabitat nominal nominal

newtype IsAValidatedAnimal habitat animal = IsAValidatedAnimal Defn

type role IsAValidatedAnimal nominal nominal

proveIsAValidatedAnimal ::
  String ~~ habitat ::: BelongsIn habitat animal ->
  String ~~ animal ::: IsNonEmpty animal ->
  Proof (IsAValidatedAnimal habitat animal)
proveIsAValidatedAnimal _ _ = axiom

proveHasAccessToHabitat :: LoggedInUser ~~ user -> String ~~ habitat -> Maybe (Proof (HasAccessToHabitat user habitat))
proveHasAccessToHabitat user habitat =
  let LoggedInUser _ (HabitatAccess habitats) = the user
   in if the habitat `elem` NonEmpty.toList habitats
        then Just axiom
        else Nothing

proveInHabitat :: String ~~ animal -> String ~~ habitat -> Maybe (Proof (BelongsIn habitat animal))
proveInHabitat animal habitat
  | the animal == "polarbear" && the habitat == "north-pole" =
    Just axiom
  | the animal == "seal" && the habitat == "north-pole" =
    Just axiom
  | the animal == "lion" && the habitat == "savanna" = Just axiom
  | the animal == "pumba" && the habitat == "savanna" = Just axiom
  | otherwise = Nothing

proveHasAMaximumLengthOf :: [a] ~~ l -> Int ~~ i -> Maybe (Proof (HasAMaximumLengthOf i l))
proveHasAMaximumLengthOf list size =
  if length (the list) <= the size then Just axiom else Nothing

takeXElements :: Int ~~ size ::: IsPositive size -> [a] -> [a] ? HasAMaximumLengthOf size
takeXElements s x =
  let takenList = take (the s) x
   in assert takenList

instance ProvableInIsolation (String ~~ n) (IsNonEmpty n) where
  proveInIsolation s =
    case proveIsNonEmpty s of
      Just p -> Right p
      Nothing -> Left "could not prove string nonempty!"

instance ProvableInIsolation (String ~~ n) (IsTrimmed n) where
  proveInIsolation s =
    case proveIsTrimmed s of
      Just p -> Right p
      Nothing -> Left "could not prove string trimmed!"

instance ProvableInIsolation (Int ~~ n) (IsPositive n) where
  proveInIsolation s =
    case proveIsPositive s of
      Just p -> Right p
      Nothing -> Left "could not prove int positive!"

proveIsNonEmpty :: String ~~ n -> Maybe (Proof (IsNonEmpty n))
proveIsNonEmpty x =
  if not (null (the x)) then Just axiom else Nothing

proveIsTrimmed :: String ~~ n -> Maybe (Proof (IsTrimmed n))
proveIsTrimmed x =
  if stripStart (stripEnd $ stringToText $ the x) == stringToText (the x)
    then Just axiom
    else Nothing

proveIsPositive :: Int `Named` n -> Maybe (Proof (IsPositive n))
proveIsPositive x =
  if the x > 0 then Just axiom else Nothing
