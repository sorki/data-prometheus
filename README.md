# data-prometheus

Pure Prometheus metrics parser and builder.

## Usage

### Parsing metrics

```haskell
import Data.Prometheus

import Network.Wreq

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  r <- get "http://localhost:9100/metrics"
  case parseProm (BL.toStrict $ r ^. responseBody) of
    Right result -> print result
    Left err -> putStrLn err
```

### Generating metrics

```haskell
import Data.Prometheus
import Data.Function ((&))

main :: IO ()
main = do
  runMetrics $ do
    addMetric
      (metric "sample" & desc "sample metric")
      (Counter 13)

    logError "something is not right"

    addMetric
      (metric "sample" & sub "gauge" & label "key" "val")
      (Gauge 13)
```
