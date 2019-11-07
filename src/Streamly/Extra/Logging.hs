module Streamly.Extra.Logging where

import           Control.Monad.Catch     (MonadMask)
import           Control.Monad.IO.Class  (MonadIO)
import qualified Control.Monad.Log       as L
import qualified Control.Monad.Log.Label as Label
import           Data.Text               (Text)
import           Prelude                 hiding (log)

logger :: MonadIO m => Text -> m (L.Logger Label.Label)
logger label =
  L.makeDefaultLogger
    "%FT%T%z"
    (L.LogStderr 4096)
    L.levelDebug
    (Label.Label label)

log ::
     (MonadIO m, MonadMask m)
  => (t -> L.LogT Label.Label m b)
  -> Text
  -> t
  -> m b
log level label msg = do
  l <- logger label
  L.runLogTSafe l $ level msg

type LogFn m
   = (MonadIO m, MonadMask m) =>
       Text -> Text -> m ()

debug :: LogFn m
debug = log L.debug

info :: LogFn m
info = log L.info

warning :: LogFn m
warning = log L.warning

error :: LogFn m
error = log L.error

critical :: LogFn m
critical = log L.critical

metricsInfo :: Text
metricsInfo = "Metrics"

userInputWarning :: Text
userInputWarning = "User input"
