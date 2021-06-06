{-# LANGUAGE LambdaCase #-}

module Effects.Logging.Graylog.GraylogServer where

import Control.Exception
import Data.List
import Network.Socket

-- | The maximum size of each datagram when using UDP transit methods.
type ChunkSize = Word

data GraylogServer = GraylogServer
  { _graylogHost :: String,
    _graylogPort :: String,
    _graylogAddress :: AddrInfo,
    _graylogSocket :: Socket,
    _graylogChunkSize :: ChunkSize
  }

openGraylog ::
  -- | The host on which graylog is running.
  HostName ->
  -- | The port on which graylog is running.
  ServiceName ->
  -- | The maximum size of each UDP datagram.
  ChunkSize ->
  IO (Either String GraylogServer)
openGraylog h p cksize
  | cksize < 1024 = return $ Left "ChunkSize must be at least 1024."
  | otherwise = do
    result <-
      try $
        getAddrInfo Nothing (Just h) (Just p)
          >>= ( \case
                  [] -> return $ Left "No address info found."
                  infos ->
                    case find (\i -> addrSocketType i == Datagram) infos of
                      Nothing -> return $ Left "No datagram info found for address."
                      Just i -> do
                        sock <- socket (addrFamily i) Datagram defaultProtocol
                        connect sock (addrAddress i)
                        return $ Right $ GraylogServer h p i sock cksize
              ) ::
        IO (Either IOException (Either String GraylogServer))
    case result of
      Left _ -> return $ Left "Unknown error"
      Right r -> return r

closeGraylog :: GraylogServer -> IO ()
closeGraylog server = close $ _graylogSocket server
