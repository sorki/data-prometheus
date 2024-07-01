module Data.Prometheus.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)

data MetricId = MetricId
  { metricIdName :: ByteString
  , metricIdHelp :: ByteString
  , metricIdLabels :: Map ByteString ByteString }
  deriving (Eq, Ord, Show)

data Metric
  = Counter Double
  | Gauge Double
  | Summary
    { sumQuantiles :: Map Double Double
    , sumSum       :: Double
    , sumCount     :: Double
    }
  | Histogram
    { histBuckets :: Map Double Double
    , histSum     :: Double
    , histCount   :: Double
    }
  deriving (Eq, Ord, Show)
