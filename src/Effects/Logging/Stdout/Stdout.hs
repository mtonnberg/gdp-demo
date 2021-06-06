module Effects.Logging.Stdout.Stdout where

import Control.Monad.Logger
import qualified Data.ByteString.Char8 as S8
import System.IO (Handle)

defaultOutput :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultOutput h loc src level msg = S8.hPutStr h ls
  where
    ls = defaultLogStrBS loc src level msg

defaultLogStrBS :: Loc -> LogSource -> LogLevel -> LogStr -> S8.ByteString
defaultLogStrBS a b c d = toBS $ defaultLogStr a b c d
  where
    toBS = fromLogStr

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level =
  case level of
    LevelOther t -> toLogStr t
    _ -> toLogStr $ S8.pack $ drop 5 $ show level
