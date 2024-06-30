{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Monad where

import qualified Data.Map.Strict       as M
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans.State.Strict

import Data.Prometheus.Types
import Data.Prometheus.Pretty

data MetricState = MetricState
  { metrics :: PromMetrics
  , errors  :: [B.ByteString]
  }

type MetricsT m = StateT MetricState m ()

-- | Evaluate metrics into `MetricState`
execMetrics :: Monad m => MetricsT m -> m MetricState
execMetrics = flip execStateT (MetricState M.empty [])

-- | Evaluate metrics and return pretty-printed output
-- as expected by textfile collector
runMetrics :: Monad m => MetricsT m -> m B.ByteString
runMetrics x = do
  ms <- execMetrics x
  return $ B.concat [ prettyMetrics (metrics ms),  B.unlines (errors ms) ]

-- | Add metric with value
addMetric :: Monad m => MetricId -> Metric -> MetricsT m
addMetric mId mData = modify $ \ms -> ms { metrics = M.insert mId mData (metrics ms) }

-- | Log error message
--
-- These are appended after all metrics were printed
--
-- Not a standard token but textfile collector ignores it as a comment
-- and we can use it to provide some insight to our scripts.
logError :: Monad m => B.ByteString -> StateT MetricState m ()
logError err = modify $ \ms -> ms { errors = (errors ms) ++ [errComment] }
  where errComment = B.unwords [ "# ERROR", err ]

-- | Create metric with just `name`
metric :: B.ByteString -> MetricId
metric mName = MetricId mName B.empty M.empty

-- | Append `subName` to name of the MetricId
--
-- > metric "a" & sub "b"
-- results in name "a_b"
sub :: B.ByteString -> MetricId -> MetricId
sub subName m = m { name = (name m) <> "_" <> subName }

-- | Set help text / description of MetricId
desc :: B.ByteString -> MetricId -> MetricId
desc h m = m { help = h }

-- | Add label to MetricId
label :: B.ByteString -> B.ByteString -> MetricId -> MetricId
label k v m = m { labels = M.insert k v (labels m) }

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

-- | Convert Enum to Gauge, 0 meaning Ok status
enumToGauge :: Enum a => a -> Metric
enumToGauge = Gauge . fromIntegral . fromEnum
