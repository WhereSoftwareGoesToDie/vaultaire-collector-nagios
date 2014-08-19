-- | Does all the processing/queueing

{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Nagios.Perfdata.Collector.Process where

import Data.Nagios.Perfdata.Collector.Cache
import Data.Nagios.Perfdata.Collector.Rep
import Data.Nagios.Perfdata.Collector.Util

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Word
import Data.Bifunctor (second)
import Data.Maybe
import Data.Either (partitionEithers)
import Data.Set hiding (map,partition)
import Data.IORef
import qualified Data.Text as T

import Data.Nagios.Perfdata
import Data.Nagios.Perfdata.Metric
import Marquise.Client

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum. Does not queue redundant updates
queueDatumSourceDict :: Perfdata -> Collector ()
queueDatumSourceDict datum = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    hashes <- liftIO $ readIORef collectorHashes
    let metrics = map (second metricUOM) $ perfdataMetrics datum
    let (hashUpdates, sdUpdates) = unzip $ mapMaybe (getChanges optNormalise hashes) metrics
    let newHashes = fromList hashUpdates `union` hashes
    liftIO $ writeIORef collectorHashes newHashes
    mapM_ (logDebugN . T.pack . (\x -> "Writing source dict: " ++ show x)) sdUpdates
    mapM_ (uncurry $ maybeUpdate collectorSpoolFiles) sdUpdates
  where
    getChanges :: Bool -> Set Word64 -> (String, UOM) -> Maybe (Word64, (Address, Either String SourceDict))
    getChanges normalise hashes (metric, uom)
        | member currentHash hashes = Nothing
        | otherwise = changes
            where
                assocList = buildList datum metric uom normalise
                currentHash = hashList assocList
                changes = Just (currentHash, (getAddress datum metric, getSourceDict assocList))
    maybeUpdate spool addr sd =
        case sd of
            Left err -> logWarnN $ T.pack $ "Error updating source dict: " ++ show err
            Right dict -> liftIO $ queueSourceDictUpdate spool addr dict

processDatum :: Perfdata -> Collector ()
processDatum datum = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    let (badPoints, goodPoints) = partitionPoints . unpackMetrics $ datum
    logDebugN $ T.pack $ concat ["Decoded datum: ", show datum, " - ", show (length goodPoints), " valid metrics"]
    mapM_ (logWarnN . T.pack . flip (++) "Warning: failed decoding datapoint: ") badPoints
    mapM_ (uncurry (sendPoint collectorSpoolFiles (datumTimestamp datum))) goodPoints
    queueDatumSourceDict datum
  where
    partitionPoints = partitionEithers . map shiftEither
    shiftEither :: (a, Either b c) -> Either b (a,c)
    shiftEither (x, y) = second (x,) y
    sendPoint spool ts addr point = do
        logDebugN $ T.pack $ concat["Writing datum ", show point, " with address ", show addr, " and timestamp ", show ts]
        liftIO $ queueSimple spool addr ts point
    datumTimestamp = TimeStamp . fromIntegral . perfdataTimestamp
