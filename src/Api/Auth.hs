{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Auth
  ( authHandler,
    LoggedInUser (..),
    UserId (..),
    UserName,
    HabitatAccess (..),
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Data.Typeable (Typeable)
import DomainIndependent.StringConversions (strictByteStringToText)
import GDP (Defn, defn, type (~~))
import GHC.Generics (Generic (..))
import Network.Wai
  ( Request,
    queryString,
  )
import Servant
import Servant.Foreign
  ( Foreign,
    HasForeign,
    foreignFor,
  )
import Servant.Server.Experimental.Auth
  ( AuthHandler,
    AuthServerData,
    mkAuthHandler,
  )

newtype UserId
  = UserId Text
  deriving (Show, Generic, Ord, Eq)

newtype HabitatAccess
  = HabitatAccess (NonEmpty String)
  deriving (Show, Generic, Ord, Eq)

data LoggedInUser
  = LoggedInUser UserId HabitatAccess
  deriving (Show, Typeable, Generic, Ord, Eq)

type instance AuthServerData (AuthProtect "normalUser") = LoggedInUser ~~ UserName

instance
  (HasForeign lang ftype sublayout) =>
  HasForeign lang ftype (AuthProtect "normalUser" :> sublayout)
  where
  type Foreign ftype (AuthProtect "normalUser" :> sublayout) = Foreign ftype sublayout
  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy sublayout)

lookupAccount ::
  Text ->
  IO (Maybe LoggedInUser)
lookupAccount u =
  case u of
    "user1" ->
      return $ Just $ LoggedInUser (UserId "user1") (HabitatAccess $ NonEmpty.fromList ["north-pole"])
    "user2" ->
      return $ Just $ LoggedInUser (UserId "user2") (HabitatAccess $ NonEmpty.fromList ["savanna"])
    _ -> return Nothing

authHandler ::
  AuthHandler Request (LoggedInUser ~~ UserName)
authHandler =
  mkAuthHandler normalAccessHandler

newtype UserName = UserName Defn

type role UserName

normalAccessHandler ::
  Request ->
  Handler (LoggedInUser ~~ UserName)
normalAccessHandler req =
  let tokenAsByteString :: Maybe Text
      tokenAsByteString =
        ("user" `lookupQ` req)
      lookedUpAccount :: Handler (Maybe LoggedInUser)
      lookedUpAccount =
        case tokenAsByteString of
          Just t -> liftIO $ lookupAccount t
          Nothing -> return Nothing
   in maybe (throw403 "Could not verify access-token") (return . defn) =<< lookedUpAccount

throw403 :: MonadError ServerError m => BL.ByteString -> m a
throw403 msg = throwError (err403 {errBody = msg})

lookupQ :: BS.ByteString -> Request -> Maybe Text
lookupQ queryKey req =
  fmap strictByteStringToText $
    snd
      =<< find (\(k, _) -> k == queryKey) (queryString req)
