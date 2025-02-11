module Data.Prometheus
  ( parseProm
  , runMetrics
  , filterMetrics
  , findMetrics
  , hasLabel
  , byLabel
  , byLabel'
  , module Data.Prometheus.Monad
  , module Data.Prometheus.Parse
  , module Data.Prometheus.Pretty
  , module Data.Prometheus.Types
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString.Char8
import qualified Data.Map
import Data.Prometheus.Monad
import Data.Prometheus.Parse
import Data.Prometheus.Pretty
import Data.Prometheus.Types

-- | Parse Prometheus metrics from ByteString
parseProm
  :: ByteString
  -> Either String (Map MetricId Metric)
parseProm = parseOnly parseMetrics

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

