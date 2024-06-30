{-# LANGUAGE OverloadedStrings #-}
module Data.Prometheus.Parse where

import Control.Applicative

import Prelude hiding (takeWhile)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map

import Data.Prometheus.Types

parseMetrics :: Parser (Map MetricId Metric)
parseMetrics = Data.Map.fromList . concat <$>
  many1 parseMetric <* many parseError <* endOfInput

parseMetric :: Parser [(MetricId, Metric)]
parseMetric = do
  (name, help, typ) <- parseMeta
  lm <- case typ of
    "counter" -> parseCounters
    "gauge" -> parseGauges
    "untyped" -> parseGauges -- untyped /o\
    "histogram" -> parseHistogram
    "summary" -> parseSummary
    x -> fail $ show x

  return $ map (\(labels, metric) -> (MetricId name help labels, metric)) lm

-- name, help, textual type
parseMeta :: Parser (ByteString, ByteString, ByteString)
parseMeta = do
  "# HELP "
  name <- word
  space
  help <- eol
  endOfLine
  "# TYPE "
  word
  space
  typ <- word
  endOfLine
  return (name, help, typ)
  where
    eol :: Parser ByteString
    eol = takeWhile (/= '\n')

    word :: Parser ByteString
    word  = takeWhile1 (\x -> x /=' ' && x /= '\n')

parseGauges :: Parser [(Map ByteString ByteString, Metric)]
parseGauges = many1 (labelsValue (Gauge <$> double))

parseCounters :: Parser [(Map ByteString ByteString, Metric)]
parseCounters = many1 (labelsValue (Counter <$> double))

parseSummary :: Parser [(Map ByteString ByteString, Metric)]
parseSummary = do
  qs <- Data.Map.fromList <$> parseQuantiles `sepBy` endOfLine <?> "quantiles"
  (_, sum) <- labelsValue double
  (_, cnt) <- labelsValue double
  return $ [(mempty, Summary qs sum cnt)]

parseQuantiles :: Parser (Double, Double)
parseQuantiles = do
  takeWhile1 (\x -> x /= '{' && x /= ' ')
  q <- "{quantile=\"" *> double <* "\"}"
  space
  val <- double
  return (q, val)

parseHistogram :: Parser [(Map ByteString ByteString, Metric)]
parseHistogram = do
  qs <- Data.Map.fromList <$> parseHistBuckets `sepBy` endOfLine <?> "quantiles"
  (_, sum) <- labelsValue double
  (_, cnt) <- labelsValue double
  return $ [(mempty, Histogram qs sum cnt)]

parseHistBuckets :: Parser (Double, Double)
parseHistBuckets = do
  takeWhile1 (\x -> x /= '{' && x /= ' ')
  q <- "{le=\"" *> double <* "\"}"
  space
  val <- double
  return (q, val)

labelsValue :: Parser b -> Parser (Map ByteString ByteString, b)
labelsValue f = do
  takeWhile1 (\x -> x /= '{' && x /= ' ')
  ls <- option mempty (char '{' *> parseLabels <* char '}')
  space
  val <- f
  endOfLine
  return (ls, val)

parseLabels :: Parser (Map ByteString ByteString)
parseLabels = Data.Map.fromList <$> parseLabel `sepBy1` (char ',')

parseLabel :: Parser (ByteString, ByteString)
parseLabel = do
  l <- takeWhile (/= '=')
  char '='
  v <- char '"' *> takeWhile (\x -> x /= '"') <* char '"'
  return (l, v)

parseError :: Parser ByteString
parseError = do
  "# ERROR "
  err <- eol
  endOfLine
  return err
  where
    eol :: Parser ByteString
    eol = takeWhile (/= '\n')
