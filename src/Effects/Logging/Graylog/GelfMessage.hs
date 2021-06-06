{-# LANGUAGE OverloadedStrings #-}

module Effects.Logging.Graylog.GelfMessage where

import Control.Monad.Logger (LogLevel (..))
------------------------------------------------------------------------------
import Data.Aeson
  ( ToJSON (toJSON),
    Value (Number),
    encode,
    object,
    (.=),
  )
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Effects.Logging.LogSettings (Environment (..), envToText)

------------------------------------------------------------------------------
data Level
  = Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Informational
  | Debug

instance ToJSON Level where
  toJSON lvl =
    case lvl of
      Emergency -> Number 0
      Alert -> Number 1
      Critical -> Number 2
      Error -> Number 3
      Warning -> Number 4
      Notice -> Number 5
      Informational -> Number 6
      Debug -> Number 7

-- TODO: We should verify that the name of additional fields is valid,
-- i.e. not "id" and matches the regular expression ^[\w\.\-]*$
newtype AdditionalFieldName = AdditionalFieldName
  { unAdditionalFieldName :: T.Text
  }

data AdditionalFieldValue
  = TLit T.Text
  | DLit Double

instance ToJSON AdditionalFieldValue where
  toJSON (TLit text) = toJSON text
  toJSON (DLit number) = toJSON number

data GelfMessage = GelfMessage
  { _gelfVersion :: T.Text,
    _gelfHost :: T.Text,
    _gelfShortMessage :: T.Text,
    _gelfFullMessage :: Maybe T.Text,
    -- Should be set to seconds since UNIX epoch with
    -- optional decimal places for milliseconds
    _gelfTimestamp :: Maybe Float,
    _gelfLevel :: Maybe Level,
    _gelfAdditionalFields :: [(AdditionalFieldName, AdditionalFieldValue)]
  }

instance ToJSON GelfMessage where
  toJSON msg = object fields
    where
      consMay attr = maybe id ((:) . (attr .=))
      conss =
        consMay "full_message" (_gelfFullMessage msg)
          . consMay "timestamp" (_gelfTimestamp msg)
      fields =
        conss
          [ "version" .= _gelfVersion msg,
            "host" .= _gelfHost msg,
            "short_message" .= _gelfShortMessage msg,
            "level" .= fromMaybe Alert (_gelfLevel msg)
          ]
          ++ fmap
            (\(k, v) -> unAdditionalFieldName k .= v)
            (_gelfAdditionalFields msg)

instance Show GelfMessage where
  show = LBS.unpack . encode . toJSON

toGelfMessage ::
  LogLevel ->
  T.Text ->
  T.Text ->
  [(T.Text, T.Text)] ->
  Environment ->
  T.Text ->
  GelfMessage
toGelfMessage lvl host src additionalFields environment msg =
  GelfMessage
    { _gelfVersion = "3.0",
      _gelfHost = host,
      _gelfShortMessage = msg,
      _gelfFullMessage = Nothing,
      _gelfTimestamp = Nothing,
      _gelfLevel = Just $ toGelfLevel lvl,
      _gelfAdditionalFields =
        mkAdditionalTextField "environment" (envToText environment) :
        mkAdditionalTextField "source" src :
        map (uncurry mkAdditionalTextField) additionalFields
    }
  where
    mkAdditionalTextField key value = (AdditionalFieldName key, TLit value)

toGelfLevel :: LogLevel -> Level
toGelfLevel LevelDebug = Debug
toGelfLevel LevelInfo = Informational
toGelfLevel LevelWarn = Warning
toGelfLevel LevelError = Error
toGelfLevel (LevelOther _) = Notice
