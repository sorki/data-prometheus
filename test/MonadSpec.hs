{-# LANGUAGE OverloadedStrings #-}

module MonadSpec where

import SpecHelper
import Data.Function ((&))
import qualified Data.Map as M

complex = metric "test"
            & sub "sub1" & sub "sub2"
            & desc "desc" & label "a" "b"

spec :: Spec
spec = do
  it "generates metrics" $ do
    runMetrics $ addMetric (metric "test") (Counter 7)
    `shouldBe`
    "# HELP test \n# TYPE test counter\ntest 7.0\n"

  it "generates complex metrics" $ do
    runMetrics $ addMetric complex (Counter 7)
    `shouldBe`
    "# HELP test_sub1_sub2 desc\n# TYPE test_sub1_sub2 counter\ntest_sub1_sub2{a=\"b\"} 7.0\n"

  it "adds errors" $ do
    runMetrics $ addMetric (metric "test") (Counter 7) >> logError "fail"
    `shouldBe`
    "# HELP test \n# TYPE test counter\ntest 7.0\n# ERROR fail\n"

  it "roundtrips complex" $ do
    parseProm (runMetrics (addMetric complex (Counter 7)))
    `shouldBe`
    Right (M.fromList [(complex, Counter 7)])

  it "roundtrips complex handles errors" $ do
    parseProm (runMetrics (addMetric complex (Counter 7) >> logError "nada"))
    `shouldBe`
    Right (M.fromList [(complex, Counter 7)])

  it "roundtrips multiple complex" $ do
    parseProm (runMetrics (addMetric complex (Counter 7) >> addMetric (complex & sub "xy") (Gauge 13.37)))
    `shouldBe`
    Right (M.fromList [(complex, Counter 7), (complex & sub "xy", Gauge 13.37)])

main :: IO ()
main = do
  hspec spec

