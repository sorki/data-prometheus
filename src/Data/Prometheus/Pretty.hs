{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Prometheus.Pretty where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.String (IsString)
import qualified Data.ByteString.Char8
import qualified Data.Map

import Data.Prometheus.Types

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
    helpTypeOnce prevName mid@MetricId{..} x | prevName /= name =
      (name, prettyMetric mid x)
    helpTypeOnce prevName mid@MetricId{..} x | otherwise        =
      (name, prettyMetricShort mid x <> "\n")

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
  Data.ByteString.Char8.unwords
    [ prettyId mId
    , prettyData mData
    ]

prettyHelp
  :: MetricId
  -> ByteString
prettyHelp MetricId{..} =
  Data.ByteString.Char8.unwords
    [ "# HELP"
    , name
    , help
    ]

prettyType
  :: MetricId
  -> Metric
  -> ByteString
prettyType mId x =
  Data.ByteString.Char8.unwords
  [ "# TYPE"
  , name mId
  , toTypeStr x
  ]

toTypeStr
  :: IsString p
  => Metric
  -> p
toTypeStr (Counter _)   = "counter"
toTypeStr (Gauge _)     = "gauge"
toTypeStr (Summary _ _ _)   = "summary"
toTypeStr (Histogram _ _ _) = "histogram"

prettyData :: Metric -> ByteString
prettyData (Counter c) = Data.ByteString.Char8.pack $ show c
prettyData (Gauge g) = Data.ByteString.Char8.pack $ show g
prettyData _ = error "Unable to print summary or histogram"

prettyId
  :: MetricId
  -> ByteString
prettyId MetricId{..} =
  mconcat
    [ name
    , prettyLabels labels
    ]

prettyLabels
  :: Map ByteString ByteString
  -> ByteString
prettyLabels labels | Data.Map.null labels = mempty
prettyLabels labels                 =
  mconcat
    [ "{"
    , Data.ByteString.Char8.intercalate ","
        $ Data.Map.elems
        $ Data.Map.mapWithKey
            (\k v -> mconcat [k, "=\"", v, "\""]) labels
    , "}"
    ]
