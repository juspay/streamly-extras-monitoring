{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Streamly.Extra.Metrics where

import           BasicPrelude                      (tshow)
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Reader              (MonadReader)
import           Data.Functor                      (($>))
import           Data.Maybe                        (fromMaybe, isJust)
import           Data.Text                         (Text)
import           Network.Wai.Handler.Warp          (run)
import qualified Network.Wai.Middleware.Prometheus as PM
import qualified Prometheus                        as P
import           Streamly                          (MonadAsync, SerialT)
import qualified Streamly.Extra                    as SE
import           Streamly.Extra.Logging            (info, metricsInfo)
import           Streamly.Internal                 (Fold (..))
import qualified Streamly.Prelude                  as SP

type MetricName = Text

type MetricHelp = Text

data Metric
  = Counter P.Counter
  | Gauge P.Gauge

-- makes and registers a counter
counter :: MonadIO m => MetricName -> MetricHelp -> m Metric
counter name help = do
  let c = P.counter (P.Info name help)
  rc <- P.register c
  pure $ Counter rc

-- makes and registers a gauge
gauge :: MonadIO m => MetricName -> MetricHelp -> m Metric
gauge name help = do
  let g = P.gauge (P.Info name help)
  rg <- P.register g
  pure $ Gauge rg

data LoggerDetails =
  LoggerDetails
    { label        :: Text
    , tag          :: Text
    , unit         :: Text
    , action       :: Text
    , intervalSecs :: Double
    , metrics      :: [(Metric, Maybe (Double -> Double))]
    }

-- gauges are set with rate/sec
streamlyInfoLogger :: SE.Logger LoggerDetails
streamlyInfoLogger LoggerDetails {..} _ n = do
  mapM_ (go intervalSecs (fromIntegral n)) metrics
  where
    go timeInterval val (metric, maybeOp) = do
      let val' = (fromMaybe id maybeOp) val
          ratePerSec = val' / timeInterval
      case metric of
        Counter c -> void $ P.addCounter c val'
        Gauge g   -> P.setGauge g ratePerSec
      info label $
        tag <> " " <> action <> " at the rate of " <> tshow ratePerSec <> " " <>
        unit <>
        "/sec"

-- run this inside a forkIO
initMetricsServer :: Int -> IO ()
initMetricsServer port = do
  info metricsInfo $
    "Starting metrics server at http://localhost:" <> tshow port <> "/"
  -- send the logging interval
  run port (PM.prometheus PM.def PM.metricsApp)

-- `withRateGauge`, by its nature, outputs an infinite stream even if the input stream is finite.
-- This function outputs a finite stream if the input stream is finite.
finiteWithRateGauge ::
     forall m a logger. MonadAsync m
  => MonadReader (SE.LoggerConfig logger) m =>
       logger -> SerialT m a -> SerialT m a
finiteWithRateGauge logger s =
  SP.mapMaybe id $
  SP.takeWhile isJust $ SE.withRateGauge logger $ (Just <$> s) <> pure Nothing

-- Does @action at @interval and returns the stream as is
doAt :: (Monad m) => Int -> (a -> m ()) -> SerialT m a -> SerialT m a
doAt interval action = SP.tap (Fold step begin end)
  where
    step 0 a = action a $> interval
    step n _ = pure (n - 1)
    begin = pure interval
    end = const (pure ())

inc :: P.MonadMonitor m => Metric -> m ()
inc (Counter c) = P.incCounter c
inc (Gauge g)   = P.incGauge g

-- returns the success status of the add operation
add :: P.MonadMonitor m => Metric -> Double -> m Bool
add (Counter c) n = P.addCounter c n
add (Gauge g) n   = P.addGauge g n $> True

get :: MonadIO m => Metric -> m Double
get (Counter c) = P.getCounter c
get (Gauge g)   = P.getGauge g

setGauge :: P.MonadMonitor m => Metric -> Double -> m ()
setGauge (Gauge g) n = P.setGauge g n
setGauge _ _         = pure ()
