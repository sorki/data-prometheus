{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Prometheus.Pretty
  ( prettyMetrics
  , prettyMetric
  , prettyMetricShort
  , prettyId
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.String (IsString)
import qualified Data.ByteString.Char8
import qualified Data.Map

import Data.Prometheus.Types
import Data.Prometheus.Monad

prettyMetrics :: Map MetricId Metric -> ByteString
prettyMetrics =
    mconcat
  . Data.Map.elems
  . snd
  . Data.Map.mapAccumWithKey
      helpTypeOnce
      mempty
  where
    -- render help and metric type just once
    -- for consecutive metridId names
    -- (with only difference in labels)
    helpTypeOnce prevName mid@MetricId{..} x | prevName /= metricIdName =
      (metricIdName, prettyMetric mid x)
    helpTypeOnce _prevName mid@MetricId{..} x | otherwise =
      (metricIdName, prettyMetricShort mid x <> "\n")

prettyMetric :: MetricId -> Metric -> ByteString
prettyMetric mId mData =
  Data.ByteString.Char8.unlines
    [ prettyHelp mId
    , prettyType mId mData
    , prettyMetricShort mId mData
    ]

prettyMetricShort
  :: MetricId
  -> Metric
  -> ByteString
prettyMetricShort mId mData =
  case mData of
    Counter x -> simple mId x
    Gauge x -> simple mId x
    Summary{..} ->
      Data.ByteString.Char8.unlines
      $ [ simple
            (label
              "quantile"
              (Data.ByteString.Char8.pack $ show k)
              mId
            )
            v
        | (k, v) <- Data.Map.toList sumQuantiles
        ]
        ++
        [ simple (sub "sum" mId) sumSum
        , simple (sub "count" mId) sumCount
        ]
    Histogram{..} ->
      Data.ByteString.Char8.unlines
      $ [ simple
            (label
              "le"
              (Data.ByteString.Char8.pack $ show k)
              (sub "bucket" mId)
            )
            v
        | (k, v) <- Data.Map.toList histBuckets
        ]
        ++
        [ simple (sub "sum" mId) histSum
        , simple (sub "count" mId) histCount
        ]

  where
    simple i val =
      Data.ByteString.Char8.unwords
        [ prettyId i
        , Data.ByteString.Char8.pack $ show val
        ]

prettyHelp
  :: MetricId
  -> ByteString
prettyHelp MetricId{..} =
  Data.ByteString.Char8.unwords
    [ "# HELP"
    , metricIdName
    , metricIdHelp
    ]

prettyType
  :: MetricId
  -> Metric
  -> ByteString
prettyType mId x =
  Data.ByteString.Char8.unwords
  [ "# TYPE"
  , metricIdName mId
  , toTypeStr x
  ]

toTypeStr
  :: IsString p
  => Metric
  -> p
toTypeStr (Counter _)   = "counter"
toTypeStr (Gauge _)     = "gauge"
toTypeStr (Summary{})   = "summary"
toTypeStr (Histogram{}) = "histogram"

prettyId
  :: MetricId
  -> ByteString
prettyId MetricId{..} =
  mconcat
    [ metricIdName
    , prettyLabels metricIdLabels
    ]

prettyLabels
  :: Map ByteString ByteString
  -> ByteString
prettyLabels labels | Data.Map.null labels = mempty
prettyLabels labels | otherwise =
  mconcat
    [ "{"
    , Data.ByteString.Char8.intercalate ","
        $ Data.Map.elems
        $ Data.Map.mapWithKey
            (\k v -> mconcat [k, "=\"", v, "\""]) 
            labels
    , "}"
    ]
