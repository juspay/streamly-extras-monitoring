{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Streamly.Extra.Metrics where

import           BasicPrelude                      (tshow)
import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Reader              (MonadReader)
import           Data.Bifunctor                    (first)
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

class P.MonadMonitor m =>
      MonadMonitor m


register :: MonadIO m => Metric s -> m s
register (Metric s) = P.register s

registerIO :: MonadIO m => m (Metric s) -> m s
registerIO mMetric = mMetric >>= register

--
-- Counter
--
type Counter = P.Counter

-- makes a counter
mkCounter :: MetricName -> MetricHelp -> Metric Counter
mkCounter name = Metric . P.counter . P.Info name

-- makes and registers a counter
-- gives you `m Counter` and not `Metric Counter`
-- use this if you dont want `Metric Counter` in future, otherwise use `mkGauge`
mkRegCounter :: MonadIO m => MetricName -> MetricHelp -> m Counter
mkRegCounter name = register . mkCounter name

getCounter :: MonadIO m => Counter -> m Double
getCounter = P.getCounter

incCounter :: MonadMonitor m => Counter -> m ()
incCounter = P.incCounter

-- returns the success status of the add operation
addCounter :: MonadMonitor m => Counter -> Double -> m Bool
addCounter = P.addCounter

--
-- Gauge
--
type Gauge = P.Gauge

-- makes a gauge
mkGauge :: MetricName -> MetricHelp -> Metric Gauge
mkGauge name = Metric . P.gauge . P.Info name

-- makes and registers a gauge
-- gives you `m Gauge` and not `Metric Gauge`
-- use this if you dont want `Metric Gauge` in future, otherwise use `mkGauge`
mkRegGauge :: MonadIO m => MetricName -> MetricHelp -> m Gauge
mkRegGauge name = register . mkGauge name

getGauge :: MonadIO m => Gauge -> m Double
getGauge = P.getGauge

incGauge :: MonadMonitor m => Gauge -> m ()
incGauge = P.incGauge

addGauge :: MonadMonitor m => Gauge -> Double -> m ()
addGauge = P.addGauge

setGauge :: (MonadMonitor m) => Gauge -> Double -> m ()
setGauge = P.setGauge

--
-- Vector
--
class P.Label l =>
      Label l


type Vector = P.Vector

-- makes a vector
mkVector :: Label l => l -> Metric s -> Metric (Vector l s)
mkVector label (Metric s) = Metric . P.vector label $ s

-- makes and registers a vector
mkRegVector :: (MonadIO m, Label l) => l -> Metric a -> m (Vector l a)
mkRegVector label = register . mkVector label

withLabel ::
     (Label l, MonadMonitor m) => Vector l s -> l -> (s -> IO ()) -> m ()
withLabel = P.withLabel

removeLabel :: (Label l, MonadMonitor m) => Vector l s -> l -> m ()
removeLabel = P.removeLabel

clearLabels :: (Label l, MonadMonitor m) => Vector l s -> m ()
clearLabels = P.clearLabels

getVectorWith :: Vector l s -> (s -> IO a) -> IO [(l, a)]
getVectorWith = P.getVectorWith

--
-- Metric
--
newtype Metric s =
  Metric (P.Metric s)

type MaybeUpdateFn = Maybe (Double -> Double)

data MetricDetails =
  MetricDetails
    { counters :: [(Counter, MaybeUpdateFn)]
    , gauges   :: [(Gauge, MaybeUpdateFn)]
    }

-- run this inside a forkIO
initMetricsServer :: Int -> IO ()
initMetricsServer port = do
  info metricsInfo $
    "Starting metrics server at http://localhost:" <> tshow port <> "/"
  run port (PM.prometheus PM.def PM.metricsApp)

--
-- Metrics Logger
--
data LoggerDetails =
  LoggerDetails
    { label        :: Text
    , tag          :: Text
    , unit         :: Text
    , action       :: Text
    , intervalSecs :: Double
    , metrics      :: MetricDetails
    }

defaultLoggerDetails :: LoggerDetails
defaultLoggerDetails =
  LoggerDetails
    { label = "defaultLabel"
    , tag = "defaultTag"
    , unit = "defaultUnit"
    , action = "defaultAction"
    , intervalSecs = 1.0
    , metrics = MetricDetails {counters = [], gauges = []}
    }

data M
  = C Counter
  | G Gauge

-- gauges are set with rate/sec
streamlyInfoLogger :: (MonadMonitor IO) => SE.Logger LoggerDetails
streamlyInfoLogger LoggerDetails {..} _ n = do
  mapM_ (update intervalSecs (fromIntegral n)) (first C <$> counters metrics)
  mapM_ (update intervalSecs (fromIntegral n)) (first G <$> gauges metrics)
  where
    update timeInterval val (metric, maybeOp) = do
      let val' = fromMaybe id maybeOp $ val
          ratePerSec = val' / timeInterval
      case metric of
        C c -> void $ addCounter c val'
        G g -> setGauge g ratePerSec
      info label $
        tag <> " " <> action <> " at the rate of " <> tshow ratePerSec <> " " <>
        unit <>
        "/sec"

--
-- Streamly Metrics
--
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
