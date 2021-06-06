{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module DomainIndependent.Choice where

import Data.Aeson (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.Bifunctor (first, second)
import GHC.Generics (Generic)

data Choice2 a b
  = FirstOf2 a
  | SecondOf2 b
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Choice3 a b c
  = FirstOf3 a
  | SecondOf3 b
  | ThirdOf3 c
  deriving (Eq, Show, Generic, ToJSON)

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (Choice3 a b c) where
  parseJSON =
    withObject "choice3" $ \o -> do
      tagName <- o .: "tag"
      let tagN :: String
          tagN = tagName
      if tagN == "FirstOf2" || tagName == "FirstOf3"
        then do
          contents <- o .: "contents"
          parsedContents <- parseJSON contents
          return $ FirstOf3 parsedContents
        else
          if tagName == "SecondOf2" || tagName == "SecondOf3"
            then do
              contents <- o .: "contents"
              parsedContents <- parseJSON contents
              return $ SecondOf3 parsedContents
            else
              if tagName == "ThirdOf3"
                then do
                  contents <- o .: "contents"
                  parsedContents <- parseJSON contents
                  return $ ThirdOf3 parsedContents
                else fail "Couldn't decode Choice3"

data Choice4 a b c d
  = FirstOf4 a
  | SecondOf4 b
  | ThirdOf4 c
  | FourthOf4 d
  deriving (Eq, Show, Generic, ToJSON)

instance
  (FromJSON a, FromJSON b, FromJSON c, FromJSON d) =>
  FromJSON (Choice4 a b c d)
  where
  parseJSON =
    withObject "choice4" $ \o -> do
      tagName <- o .: "tag"
      let tagN :: String
          tagN = tagName
      if tagN == "FirstOf2" || tagName == "FirstOf3" || tagName == "FirstOf4"
        then do
          contents <- o .: "contents"
          parsedContents <- parseJSON contents
          return $ FirstOf4 parsedContents
        else
          if tagName == "SecondOf2"
            || tagName == "SecondOf3"
            || tagName == "SecondOf4"
            then do
              contents <- o .: "contents"
              parsedContents <- parseJSON contents
              return $ SecondOf4 parsedContents
            else
              if tagName == "ThirdOf3" || tagName == "ThirdOf4"
                then do
                  contents <- o .: "contents"
                  parsedContents <- parseJSON contents
                  return $ ThirdOf4 parsedContents
                else
                  if tagName == "FourthOf4"
                    then do
                      contents <- o .: "contents"
                      parsedContents <- parseJSON contents
                      return $ FourthOf4 parsedContents
                    else fail "Couldn't decode Choice4"

data Choice5 a b c d e
  = FirstOf5 a
  | SecondOf5 b
  | ThirdOf5 c
  | FourthOf5 d
  | FifthOf5 e
  deriving (Eq, Show, Generic, ToJSON)

instance
  (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) =>
  FromJSON (Choice5 a b c d e)
  where
  parseJSON =
    withObject "choice5" $ \o -> do
      tagName <- o .: "tag"
      let tagN :: String
          tagN = tagName
      if tagN == "FirstOf2" || tagName == "FirstOf3" || tagName == "FirstOf4" || tagName == "FirstOf5"
        then do
          contents <- o .: "contents"
          parsedContents <- parseJSON contents
          return $ FirstOf5 parsedContents
        else
          if tagName == "SecondOf2"
            || tagName == "SecondOf3"
            || tagName == "SecondOf4"
            || tagName == "SecondOf5"
            then do
              contents <- o .: "contents"
              parsedContents <- parseJSON contents
              return $ SecondOf5 parsedContents
            else
              if tagName == "ThirdOf3" || tagName == "ThirdOf4" || tagName == "ThirdOf5"
                then do
                  contents <- o .: "contents"
                  parsedContents <- parseJSON contents
                  return $ ThirdOf5 parsedContents
                else
                  if tagName == "FourthOf4" || tagName == "FourthOf5"
                    then do
                      contents <- o .: "contents"
                      parsedContents <- parseJSON contents
                      return $ FourthOf5 parsedContents
                    else
                      if tagName == "FifthOf5"
                        then do
                          contents <- o .: "contents"
                          parsedContents <- parseJSON contents
                          return $ FifthOf5 parsedContents
                        else fail "Couldn't decode Choice5"

choice2 :: (a -> c) -> (b -> c) -> Choice2 a b -> c
choice2 f _ (FirstOf2 x) = f x
choice2 _ g (SecondOf2 x) = g x

choice3 :: (a -> d) -> (b -> d) -> (c -> d) -> Choice3 a b c -> d
choice3 f _ _ (FirstOf3 x) = f x
choice3 _ g _ (SecondOf3 x) = g x
choice3 _ _ h (ThirdOf3 x) = h x

choice4 :: (a -> e) -> (b -> e) -> (c -> e) -> (d -> e) -> Choice4 a b c d -> e
choice4 f _ _ _ (FirstOf4 x) = f x
choice4 _ g _ _ (SecondOf4 x) = g x
choice4 _ _ h _ (ThirdOf4 x) = h x
choice4 _ _ _ k (FourthOf4 x) = k x

choice5 ::
  (a -> t) ->
  (b -> t) ->
  (c -> t) ->
  (d -> t) ->
  (e -> t) ->
  Choice5 a b c d e ->
  t
choice5 f _ _ _ _ (FirstOf5 x) = f x
choice5 _ f _ _ _ (SecondOf5 x) = f x
choice5 _ _ f _ _ (ThirdOf5 x) = f x
choice5 _ _ _ f _ (FourthOf5 x) = f x
choice5 _ _ _ _ f (FifthOf5 x) = f x

mapChoices2 :: (a -> a') -> (b -> b') -> Choice2 a b -> Choice2 a' b'
mapChoices2 f _ (FirstOf2 x) = FirstOf2 $ f x
mapChoices2 _ g (SecondOf2 x) = SecondOf2 $ g x

mapChoices3 ::
  (a -> a') -> (b -> b') -> (c -> c') -> Choice3 a b c -> Choice3 a' b' c'
mapChoices3 f _ _ (FirstOf3 x) = FirstOf3 $ f x
mapChoices3 _ g _ (SecondOf3 x) = SecondOf3 $ g x
mapChoices3 _ _ h (ThirdOf3 x) = ThirdOf3 $ h x

mapChoices4 ::
  (a -> a') ->
  (b -> b') ->
  (c -> c') ->
  (d -> d') ->
  Choice4 a b c d ->
  Choice4 a' b' c' d'
mapChoices4 f _ _ _ (FirstOf4 x) = FirstOf4 $ f x
mapChoices4 _ g _ _ (SecondOf4 x) = SecondOf4 $ g x
mapChoices4 _ _ h _ (ThirdOf4 x) = ThirdOf4 $ h x
mapChoices4 _ _ _ j (FourthOf4 x) = FourthOf4 $ j x

mapChoices5 ::
  (a -> a') ->
  (b -> b') ->
  (c -> c') ->
  (d -> d') ->
  (e -> e') ->
  Choice5 a b c d e ->
  Choice5 a' b' c' d' e'
mapChoices5 f _ _ _ _ (FirstOf5 x) = FirstOf5 $ f x
mapChoices5 _ f _ _ _ (SecondOf5 x) = SecondOf5 $ f x
mapChoices5 _ _ f _ _ (ThirdOf5 x) = ThirdOf5 $ f x
mapChoices5 _ _ _ f _ (FourthOf5 x) = FourthOf5 $ f x
mapChoices5 _ _ _ _ f (FifthOf5 x) = FifthOf5 $ f x

mapFirstOf2 :: (a -> a') -> Choice2 a b -> Choice2 a' b
mapFirstOf2 f = mapChoices2 f id

mapSecondOf2 :: (b -> b') -> Choice2 a b -> Choice2 a b'
mapSecondOf2 = mapChoices2 id

mapFirstOf3 :: (a -> a') -> Choice3 a b c -> Choice3 a' b c
mapFirstOf3 f = mapChoices3 f id id

mapSecondOf3 :: (b -> b') -> Choice3 a b c -> Choice3 a b' c
mapSecondOf3 f = mapChoices3 id f id

mapThirdOf3 :: (c -> c') -> Choice3 a b c -> Choice3 a b c'
mapThirdOf3 = mapChoices3 id id

mapFirstOf4 :: (a -> a') -> Choice4 a b c d -> Choice4 a' b c d
mapFirstOf4 f = mapChoices4 f id id id

mapSecondOf4 :: (b -> b') -> Choice4 a b c d -> Choice4 a b' c d
mapSecondOf4 f = mapChoices4 id f id id

mapThirdOf4 :: (c -> c') -> Choice4 a b c d -> Choice4 a b c' d
mapThirdOf4 f = mapChoices4 id id f id

mapFourthOf4 :: (d -> d') -> Choice4 a b c d -> Choice4 a b c d'
mapFourthOf4 = mapChoices4 id id id

partitionChoice2 :: [Choice2 a b] -> ([a], [b])
partitionChoice2 = foldr f ([], [])
  where
    f :: Choice2 a b -> ([a], [b]) -> ([a], [b])
    f (FirstOf2 a) = first (a :)
    f (SecondOf2 b) = second (b :)

fromEither :: Either a b -> Choice2 a b
fromEither (Left a) = FirstOf2 a
fromEither (Right b) = SecondOf2 b

toEither :: Choice2 a b -> Either a b
toEither (FirstOf2 a) = Left a
toEither (SecondOf2 b) = Right b
