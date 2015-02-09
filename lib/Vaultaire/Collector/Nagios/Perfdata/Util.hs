{-# LANGUAGE OverloadedStrings #-}

module Vaultaire.Collector.Nagios.Perfdata.Util where

import           Data.Bifunctor              (bimap, second)
import           Data.Binary.IEEE754         (doubleToWord)
import qualified Data.ByteString             as S
import qualified Data.ByteString.Char8       as C
import qualified Data.HashMap.Strict         as HashMap (fromList)
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Word

import           Data.Nagios.Perfdata
import           Data.Nagios.Perfdata.Metric
import           Marquise.Client

-- | Returns the Vaultaire SourceDict for the supplied metric in datum,
-- or an error if the relevant values have invalid characters (',' or
-- ':').
getSourceDict :: [(Text, Text)] -> Either String SourceDict
getSourceDict =
    makeSourceDict . HashMap.fromList


type MetricThresholds = (Threshold, Threshold, Threshold, Threshold)

perfdatumToThresholds :: Perfdata -> [(String, MetricThresholds)]
perfdatumToThresholds perfdata =
    map (second metricThresholds) metricList
  where
    metricList         = perfdataMetrics perfdata
    metricThresholds m = (warnValue m, critValue m, minValue m, maxValue m)

thresholdsToList :: MetricThresholds -> [(String, String)]
thresholdsToList (warn, crit, minThreshold, maxThreshold) =
    foldl filterOutNoThreshold [] $ zip nagiosPerfdataKeys [warn, crit, minThreshold, maxThreshold]
  where
    filterOutNoThreshold acc (_, NoThreshold) = acc
    filterOutNoThreshold acc (k, DoubleThreshold v) = (k, show v):acc
    nagiosPerfdataKeys = [ "_nagios_warn", "_nagios_crit", "_nagios_min", "_nagios_max" ]

-- | Builds an association list for conversion into a SourceDict
buildList :: Perfdata -> String -> UOM -> Bool -> [(Text, Text)]
buildList datum metricName uom normalise = convert $ concat [baseList, counter, unit, nagiosThresholds]
  where
    counter
        | uom == Counter = [("_counter", "1")]
        | otherwise      = []
    unit
        | normalise = [("_normalised", "1")]
        | otherwise = []
    isInteresting = (== metricName) . fst
    interestingThresholds = map snd $ filter isInteresting $ perfdatumToThresholds datum
    nagiosThresholds = concatMap thresholdsToList interestingThresholds
    -- host, metricName and service are collectively the primary key for
    -- this metric. As the nagios-perfdata package currently treats
    -- all values as floats, we also specify this as metadata for
    -- the presentation layer.
    host = perfdataHostname datum
    service = C.unpack $ perfdataServiceDescription datum
    baseList = zip ["host", "metric", "service", "_float", "_uom"] [host, metricName, service, "1", show uom]
    convert = map $ bimap T.pack fmtTag

fmtTag :: String -> Text
fmtTag = T.pack . map ensureValid

ensureValid :: Char -> Char
ensureValid ',' = '-'
ensureValid ':' = '-'
ensureValid x = x

-- | Returns the unique identifier for the named metric in the supplied
-- perfdatum. This is used to calculate the address.
getMetricId :: Perfdata -> String -> S.ByteString
getMetricId datum metric =
    let host = perfdataHostname datum in
    let service = S.unpack $ perfdataServiceDescription datum in
    "host:" <> C.pack host <> ",metric:" <> C.pack metric <> ",service:" <> S.pack service <> ","

-- | Returns the Vaultaire address for the specified metric.
getAddress :: Perfdata -> String -> Address
getAddress p = hashIdentifier . getMetricId p

-- | Given a Perfdata object, extract each metric into a list of
-- (address,value) tuples.
unpackMetrics :: Perfdata -> [(Address, Either String Word64)]
unpackMetrics datum =
    map (bimap (getAddress datum) extractValueWord) (perfdataMetrics datum)
  where
    extractValueWord :: Metric -> Either String Word64
    extractValueWord m = if unknownMetricValue m then
        Left "unknown metric value" else
        Right . doubleToWord $ metricValueDefault m 0.0
