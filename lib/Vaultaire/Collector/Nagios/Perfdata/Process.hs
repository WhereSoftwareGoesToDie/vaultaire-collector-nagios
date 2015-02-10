-- | Does all the processing/queueing

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Vaultaire.Collector.Nagios.Perfdata.Process where

import           Vaultaire.Collector.Nagios.Perfdata.Types
import           Vaultaire.Collector.Nagios.Perfdata.Util

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Bifunctor                      (second)
import           Data.Either                         (partitionEithers)
import           System.Log.Logger

import           Data.Nagios.Perfdata
import           Data.Nagios.Perfdata.Metric
import           Marquise.Client
import           Vaultaire.Collector.Common.Process

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum. Does not queue redundant updates
queueDatumSourceDict :: Perfdata -> Nagios ()
queueDatumSourceDict datum = do
    (_, NagiosOptions{..}) <- ask
    let metrics = map (second metricUOM) $ perfdataMetrics datum
    let sdUpdates = map (getUpdates optNormalise) metrics
    liftIO $ mapM_ (debugM "Process.queueDatumSourceDict" . (\x -> "Writing source dict: " ++ show x)) sdUpdates
    mapM_ (uncurry maybeUpdate) sdUpdates
  where
    getUpdates :: Bool -> (String, UOM) -> (Address, Either String SourceDict)
    getUpdates normalise (metric, uom) =
        (getAddress datum metric, getSourceDict $ buildList datum metric uom normalise)
    maybeUpdate addr sd =
        case sd of
            Left err -> liftIO $ warningM "Process.queueDatumSourceDict" $ "Error updating source dict: " ++ show err
            Right dict -> collectSource addr dict

processDatum :: Perfdata -> Nagios ()
processDatum datum = do
    (_, NagiosOptions{..}) <- ask
    let (badPoints, goodPoints) = partitionPoints . unpackMetrics $ datum
    liftIO $ debugM "Process.processDatum" $ concat ["Decoded datum: ", show datum, " - ", show (length goodPoints), " valid metrics"]
    liftIO $ mapM_ (warningM "Process.processDatum" . flip (++) "Warning: failed decoding datapoint: ") badPoints
    mapM_ (uncurry (sendPoint (datumTimestamp datum))) goodPoints
    queueDatumSourceDict datum
  where
    partitionPoints = partitionEithers . map shiftEither
    shiftEither :: (a, Either b c) -> Either b (a,c)
    shiftEither (x, y) = second (x,) y
    sendPoint ts addr point = do
        liftIO $ debugM "Process.processDatum" $ concat["Writing datum ", show point, " with address ", show addr, " and timestamp ", show ts]
        collectSimple $ SimplePoint addr ts point
    datumTimestamp = TimeStamp . fromIntegral . perfdataTimestamp
