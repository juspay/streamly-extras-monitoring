module Streamly.Extra.Logging where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger   (LogLevel (LevelOther), LogStr, LoggingT (LoggingT), defaultLoc,
                                         runStderrLoggingT, runStdoutLoggingT, toLogStr)
import qualified Data.Text              as T
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)

type Label = T.Text

type Message = T.Text

logger ::
     MonadIO m
  => (LoggingT m () -> m ())
  -> LogLevel
  -> Label
  -> Message
  -> m ()
logger logFn level label msg = do
  time <- formatTime defaultTimeLocale "%FT%T%z" <$> liftIO getCurrentTime
  logFn $
    loggerT
      (toLogStr ("[" <> T.pack time <> "] " <> "[" <> label <> "] " <> msg))
  where
    loggerT :: MonadIO m => LogStr -> LoggingT m ()
    loggerT msg' = LoggingT (\fn -> liftIO $ fn defaultLoc "" level msg')

loggerStderr :: MonadIO m => LogLevel -> Label -> Message -> m ()
loggerStderr = logger runStderrLoggingT

loggerStdout :: MonadIO m => LogLevel -> Label -> Message -> m ()
loggerStdout = logger runStdoutLoggingT

debug :: MonadIO m => Label -> Message -> m ()
debug = loggerStderr (LevelOther "DEBUG")

info :: MonadIO m => Label -> Message -> m ()
info = loggerStderr (LevelOther "INFO")

warning :: MonadIO m => Label -> Message -> m ()
warning = loggerStderr (LevelOther "WARNING")

error :: MonadIO m => Label -> Message -> m ()
error = loggerStderr (LevelOther "ERROR")

critical :: MonadIO m => Label -> Message -> m ()
critical = loggerStderr (LevelOther "CRITICAL ")

metricsInfo :: T.Text
metricsInfo = "Metrics"

userInputWarning :: T.Text
userInputWarning = "User input"
