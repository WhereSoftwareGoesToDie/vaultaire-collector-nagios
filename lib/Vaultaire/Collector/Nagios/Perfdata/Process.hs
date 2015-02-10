-- | Does all the processing/queueing

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Vaultaire.Collector.Nagios.Perfdata.Process where

import           Vaultaire.Collector.Nagios.Perfdata.Types
import           Vaultaire.Collector.Nagios.Perfdata.Util

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Bifunctor                      (second)
import           Data.Either                         (partitionEithers)
import qualified Data.Text                           as T

import           Data.Nagios.Perfdata
import           Data.Nagios.Perfdata.Metric
import           Marquise.Client

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum. Does not queue redundant updates
queueDatumSourceDict :: Perfdata -> Collector ()
queueDatumSourceDict datum = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    let metrics = map (second metricUOM) $ perfdataMetrics datum
    let sdUpdates = map (getUpdates optNormalise) metrics
    mapM_ (logDebugN . T.pack . (\x -> "Writing source dict: " ++ show x)) sdUpdates
    mapM_ (uncurry $ maybeUpdate collectorSpoolFiles) sdUpdates
  where
    getUpdates :: Bool -> (String, UOM) -> (Address, Either String SourceDict)
    getUpdates normalise (metric, uom) =
        (getAddress datum metric, getSourceDict $ buildList datum metric uom normalise)
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
