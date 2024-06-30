module Data.Prometheus.Types where

import Data.ByteString (ByteString)
import Data.Map (Map)

data MetricId = MetricId
  { name   :: ByteString
  , help   :: ByteString
  , labels :: Map ByteString ByteString }
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
