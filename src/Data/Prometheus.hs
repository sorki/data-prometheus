module Data.Prometheus
  ( parseProm
  , filterMetrics
  , findMetrics
  , hasLabel
  , byLabel
  , byLabel'
  , module T
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Prometheus.Monad  as T
import Data.Prometheus.Parse  as T
import Data.Prometheus.Pretty as T
import Data.Prometheus.Types  as T
import Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString.Char8
import qualified Data.Map

-- | Parse Prometheus metrics from ByteString
parseProm
  :: ByteString
  -> Either String (Map MetricId Metric)
parseProm = parseOnly parseMetrics

-- | Filter metrics where name is prefixed by `pattern`
filterMetrics :: ByteString -> Map MetricId a -> Map MetricId a
filterMetrics pattern =
  Data.Map.filterWithKey
    (\k _ -> pattern `Data.ByteString.Char8.isPrefixOf` (metricIdName k))

-- | Find metrics where name is equal to `pattern`
findMetrics :: ByteString -> Map MetricId a -> Map MetricId a
findMetrics pattern =
  Data.Map.filterWithKey (\k _ -> pattern == metricIdName k)

-- | Find metrics by `label`
hasLabel :: ByteString -> Map MetricId a -> Map MetricId a
hasLabel label' =
  Data.Map.filterWithKey (\k _ -> Data.Map.member label' (metricIdLabels k))

-- | Find metrics with `label` which matches `contents`
byLabel
  :: ByteString
  -> ByteString
  -> Map MetricId a
  -> Map MetricId a
byLabel label' contents = byLabel' label' (==contents)

-- | Find metrics with `label` which content satisfies `op` predicate
byLabel'
  :: ByteString
  -> (ByteString -> Bool)
  -> Map MetricId a
  -> Map MetricId a
byLabel' label' op =
  Data.Map.filterWithKey
    $ \k _ ->
      case Data.Map.lookup label' (metricIdLabels k) of
        Nothing -> False
        Just lc -> op lc

-- byLabels
