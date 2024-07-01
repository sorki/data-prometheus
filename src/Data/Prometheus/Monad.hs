{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Monad where

import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map
import qualified Data.ByteString.Char8

import Data.Prometheus.Types
import Data.Prometheus.Pretty

data MetricState = MetricState
  { metrics :: Map MetricId Metric
  , errors  :: [ByteString]
  }

type MetricsT m = StateT MetricState m ()

class ToMetrics a where
  toMetrics
    :: Monad m
    => MetricId
    -> a
    -> MetricsT m

instance ToMetrics a => ToMetrics [a] where
  toMetrics baseMetricId =
    mapM_ $ toMetrics baseMetricId

-- | Evaluate metrics into `MetricState`
execMetrics
  :: Monad m
  => MetricsT m
  -> m MetricState
execMetrics = flip execStateT (MetricState mempty mempty)

-- | Evaluate metrics and return pretty-printed output
-- as expected by textfile collector
runMetrics
  :: Monad m
  => MetricsT m
  -> m ByteString
runMetrics x = do
  ms <- execMetrics x
  pure
    $ mconcat
        [ prettyMetrics (metrics ms)
        , Data.ByteString.Char8.unlines (errors ms)
        ]

-- | Add metric with value
addMetric
  :: Monad m
  => MetricId
  -> Metric
  -> MetricsT m
addMetric mId mData =
  modify $ \ms ->
    ms { metrics = Data.Map.insert mId mData (metrics ms) }

-- | Log error message
--
-- These are appended after all metrics were printed
--
-- Not a standard token but textfile collector ignores it as a comment
-- and we can use it to provide some insight to our scripts.
logError
  :: Monad m
  => ByteString
  -> StateT MetricState m ()
logError err =
  modify $ \ms -> ms { errors = (errors ms) ++ [errComment] }
  where
    errComment =
      Data.ByteString.Char8.unwords
      [ "# ERROR"
      , err
      ]

-- | Create metric with just `name`
metric
  :: ByteString
  -> MetricId
metric mName = MetricId mName mempty mempty

-- | Append `subName` to the name of a @MetricId@
--
-- > metric "a" & sub "b"
-- results in name "a_b"
sub
  :: ByteString
  -> MetricId
  -> MetricId
sub subName m =
  m { metricIdName = metricIdName m <> "_" <> subName }

-- | Set help text / description of a @MetricId@
desc
  :: ByteString
  -> MetricId
  -> MetricId
desc h m =
  m { metricIdHelp = h }

-- | Add label to MetricId
label
  :: ByteString
  -> ByteString
  -> MetricId
  -> MetricId
label k v m =
  m { metricIdLabels = Data.Map.insert k v (metricIdLabels m) }

-- | Right is exitcode 0, Left non-zero
eitherExitCode :: Either a b -> Integer
eitherExitCode (Right _) = 0
eitherExitCode (Left _) = 1

-- | Convert Either to Gauge, 0 meaning Right
eitherToGauge :: Either a b -> Metric
eitherToGauge = Gauge . fromIntegral . eitherExitCode

-- | Convert Bool to Gauge, 0 meaning True
goodWhen :: Bool -> Metric
goodWhen True = Gauge 0
goodWhen False = Gauge 1

-- | Convert Enum to Gauge, 0 (typically) meaning Ok status
enumToGauge :: Enum a => a -> Metric
enumToGauge = Gauge . fromIntegral . fromEnum
