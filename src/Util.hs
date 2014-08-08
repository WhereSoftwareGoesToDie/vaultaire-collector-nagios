{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Bifunctor (bimap, second)
import Data.Binary.IEEE754 (doubleToWord)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HashMap(fromList)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word
import Data.Maybe

import Data.Nagios.Perfdata
import Data.Nagios.Perfdata.Metric
import Marquise.Client

(+.+) :: S.ByteString -> S.ByteString -> S.ByteString
(+.+) = S.append

-- | Returns the Vaultaire SourceDict for the supplied metric in datum,
-- or an error if the relevant values have invalid characters (',' or
-- ':'). 
getSourceDict :: [(Text, Text)] -> Either String SourceDict
getSourceDict = 
    makeSourceDict . HashMap.fromList


type MetricThresholds = (Threshold,Threshold,Threshold,Threshold)

perfdatumToThresholds :: Perfdata -> [ (String, MetricThresholds) ]
perfdatumToThresholds p =
    map (second metricThresholds) metricList
  where
    metricList         = perfdataMetrics p -- MetricList == [(String,Metric)]
    metricThresholds m = (warnValue m, critValue m, minValue m, maxValue m)

thresholdsToList :: (String, MetricThresholds) -> [ (String, String) ]
thresholdsToList (_, (warn,crit,minThreshold,maxThreshold)) =
    let maybeNagiosKeys = zip nagiosPerfdataKeys $ map maybeShow [warn,crit,minThreshold,maxThreshold] in
    [ (x,fromJust y) | (x,y) <- maybeNagiosKeys, isJust y ]
  where
    nagiosPerfdataKeys = [ "_nagios_warn", "_nagios_crit", "_nagios_min", "_nagios_max" ]
    maybeShow (DoubleThreshold v) = Just $ show v
    maybeShow NoThreshold         = Nothing

-- | Builds an association list for conversion into a SourceDict
buildList :: Perfdata -> String -> UOM -> Bool -> [(Text, Text)]
buildList datum metricName uom normalise = convert $ concat [baseList, counter, unit, nagiosPerfdata]
  where
    counter
        | uom == Counter = [("_counter", "1")]
        | otherwise      = []
    unit
        | normalise = [("_unit", "1")]
        | otherwise = []
    nagiosPerfdata = concatMap thresholdsToList interestingThresholds
    interestingThresholds = [ x | x <- perfdatumToThresholds datum, fst x == metricName ]
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
    "host:" +.+ C.pack host +.+ ",metric:" +.+ C.pack metric +.+ ",service:" +.+ S.pack service +.+ ","

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
