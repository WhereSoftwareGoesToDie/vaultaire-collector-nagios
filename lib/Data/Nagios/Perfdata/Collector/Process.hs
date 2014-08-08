-- | Does all the processing/queueing

{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Nagios.Perfdata.Collector.Process where

import Data.Nagios.Perfdata.Collector.Cache
import Data.Nagios.Perfdata.Collector.Rep
import Data.Nagios.Perfdata.Collector.Util

import System.IO
import Data.Word
import Data.Bifunctor (second)
import Data.Maybe
import Data.Either (partitionEithers)
import Data.Set hiding (map,partition)
import Data.IORef

import Data.Nagios.Perfdata
import Data.Nagios.Perfdata.Metric
import Marquise.Client

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum. Does not queue redundant updates
queueDatumSourceDict :: CollectorState -> Perfdata -> IO ()
queueDatumSourceDict CollectorState{..} datum = do
    let CollectorOptions{..} = collectorOpts
    hashes <- readIORef collectorHashes
    let metrics = map (\(s, m) -> (s, metricUOM m)) $ perfdataMetrics datum
    let (hashUpdates, sdUpdates) = unzip $ mapMaybe (getChanges optNormalise hashes) metrics
    let newHashes = fromList hashUpdates `union` hashes
    writeIORef collectorHashes newHashes
    mapM_ (maybePut optDebug) (map (\x -> "Writing source dict: " ++ show x) sdUpdates)
    mapM_ (uncurry maybeUpdate) sdUpdates
  where
    getChanges :: Bool -> Set Word64 -> (String, UOM) -> Maybe (Word64, (Address, Either String SourceDict))
    getChanges normalise hashes (metric, uom)
        | member currentHash hashes = Nothing
        | otherwise = changes
            where
                assocList = buildList datum metric uom normalise
                currentHash = hashList assocList
                changes = Just (currentHash, (getAddress datum metric, getSourceDict assocList))
    maybeUpdate addr sd =
        case sd of
            Left err -> hPutStrLn stderr $ "Error updating source dict: " ++ show err
            Right dict -> queueSourceDictUpdate collectorSpoolFiles addr dict
 
processDatum :: CollectorState -> Perfdata -> IO ()
processDatum state@CollectorState{..} datum = do
    let CollectorOptions{..} = collectorOpts
    let (badPoints, goodPoints) = partitionPoints . unpackMetrics $ datum
    maybePut optDebug $ concat ["Decoded datum: ", show datum, " - ", show (length goodPoints), " valid metrics"]
    mapM_ emitWarning badPoints
    mapM_ (uncurry (sendPoint optDebug (datumTimestamp datum))) goodPoints
    queueDatumSourceDict state datum
  where
    partitionPoints = partitionEithers . map shiftEither
    shiftEither :: (a, Either b c) -> Either b (a,c)
    shiftEither (x, y) = second (x,) y
    emitWarning err = hPutStrLn stderr $ "Warning: error decoding datapoint: " ++ err
    sendPoint debug ts addr point = do
        maybePut debug $ concat["Writing datum ", show point, " with address ", show addr, " and timestamp ", show ts]
        queueSimple collectorSpoolFiles addr ts point
    datumTimestamp = TimeStamp . fromIntegral . perfdataTimestamp
