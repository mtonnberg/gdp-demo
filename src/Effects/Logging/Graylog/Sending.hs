module Effects.Logging.Graylog.Sending where

import Data.Aeson (encode)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Word
import Effects.Logging.Graylog.GelfMessage (GelfMessage)
import Effects.Logging.Graylog.GraylogServer
  ( GraylogServer,
    closeGraylog,
    openGraylog,
    _graylogChunkSize,
    _graylogSocket,
  )
import Network.Socket.ByteString.Lazy
import System.Random

openConnectionAndSendMessage :: T.Text -> GelfMessage -> IO ()
openConnectionAndSendMessage serverUrl m = do
  graylogServer <- openGraylog (T.unpack serverUrl) "12201" 1024
  case graylogServer of
    Left _ -> print m
    Right gray -> do
      sendLog gray m
      closeGraylog gray
      return ()

sendLog :: GraylogServer -> GelfMessage -> IO ()
sendLog glog msg = do
  chunk <- chunker
  mapM_ (send $ _graylogSocket glog) chunk
  where
    raw = encode msg
    chunker = do
      groupId <- randomIO
      let groups = divide totalNum raw
      return $ append groupId groups seqNums
      where
        magic = word8 0x1e <> word8 0x0f
        seqNums = [0 ..] :: [Word8]
        totalNum =
          if excess > 0
            then count + 1
            else count
        (count, excess) = quotRem (LBS.length raw) gsize
        hlen = 12
        gsize = fromIntegral (_graylogChunkSize glog) - hlen
        divide 0 dat = [dat]
        divide num dat =
          let (pre, post) = LBS.splitAt gsize dat
           in pre : divide (num - 1) post
        append _ [] _ = []
        append _ _ [] = error "the impossible has happened."
        append gid (g : gs) (s : ss) =
          toLazyByteString
            ( magic <> word64BE gid <> word8 s <> word8 (fromIntegral totalNum)
                <> lazyByteString g
            ) :
          append gid gs ss
