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
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Domain.DomainProofs
  ( NonEmpty,
    Positive,
    BelongsIn,
    proveHasAccessToHabitat,
    HasAMaximumLengthOf,
    HasAccessToHabitat,
    IsAValidatedAnimal,
    proveInHabitat,
    proveHasAMaximumLengthOf,
    proveNonEmpty,
    proveIsAValidatedAnimal,
  )
where

import Api.Auth
  ( HabitatAccess (..),
    LoggedInUser (..),
  )
import qualified Data.List.NonEmpty as NonEmpty
import DomainIndependent.ProveInIsolation (ProvableInIsolation, proveFromHttpApiData)
import GDP (Defn, Proof, axiom, the, type (:::), type (~~))

newtype NonEmpty a = NonEmpty Defn

type role NonEmpty nominal

newtype Positive a = Positive Defn

type role Positive nominal

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
  String ~~ animal ::: NonEmpty animal ->
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

instance ProvableInIsolation (String ~~ n) (NonEmpty n) where
  proveFromHttpApiData s =
    case proveNonEmpty s of
      Just p -> p
      Nothing -> error "could not prove string nonempty!"

instance ProvableInIsolation (Int ~~ n) (Positive n) where
  proveFromHttpApiData s =
    case provePositive s of
      Just p -> p
      Nothing -> error "could not prove int positive!"

proveNonEmpty :: String ~~ n -> Maybe (Proof (NonEmpty n))
proveNonEmpty x =
  if not (null (the x)) then Just axiom else Nothing

provePositive :: Int ~~ n -> Maybe (Proof (Positive n))
provePositive x =
  if the x > 0 then Just axiom else Nothing
