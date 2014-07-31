-- This file is part of vaultaire-collector-nagios.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Cache
import Options
import State
import Util

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Word
import Data.Bifunctor (second)
import Data.Maybe
import Data.Either (partitionEithers)
import Data.Set hiding (map,partition)
import Prelude hiding (lookup)
import Data.IORef

import Data.Nagios.Perfdata
import Data.Nagios.Perfdata.Metric
import Marquise.Client

import Paths_vaultaire_collector_nagios (version)

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op@CollectorOptions{..} (CollectorMonad act) = do
    maybePut optDebug $ "Collector version " ++ show version ++ " starting."
    files <- createSpoolFiles optNamespace
    initialHashes <- getInitialCache optCacheFile (maybePut optDebug) 
    runReaderT act $ CollectorState op files initialHashes optCacheFile

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum. Does not queue redundant updates
queueDatumSourceDict :: SpoolFiles -> Perfdata -> CollectorMonad ()
queueDatumSourceDict spool datum = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    hashes <- liftIO $ readIORef $ collectorHashes
    let metrics = map (\(s, m) -> (s, metricUOM m)) $ perfdataMetrics datum
    let (hashUpdates, sdUpdates) = unzip $ mapMaybe (getChanges hashes optNormalise) metrics
    let newHashes = fromList hashUpdates `union` hashes
    liftIO $ do
        writeIORef collectorHashes newHashes
        mapM_ (uncurry maybeUpdate) sdUpdates
  where
    getChanges :: Set Word64 -> Bool -> (String, UOM) -> Maybe (Word64, (Address, Either String SourceDict))
    getChanges hashes normalise (metric, uom)
        | member currentHash hashes = Nothing
        | otherwise = changes
            where
                assocList = buildList datum metric uom normalise
                currentHash = hashList assocList
                changes = Just (currentHash, (getAddress datum metric, getSourceDict assocList))
    maybeUpdate addr sd =
        case sd of
            Left err -> hPutStrLn stderr $ "Error updating source dict: " ++ show err
            Right dict -> queueSourceDictUpdate spool addr dict

-- | Given a line formatted according to the standard Nagios perfdata
-- template, a) queue it for writing to Vaultaire and b) queue an update
-- for each metric's metadata.
processLine :: ByteString -> CollectorMonad ()
processLine line = do
    CollectorState{..} <- ask
    putDebugLn $ "Decoding line: " ++ show line
    case perfdataFromDefaultTemplate line of
        Left err -> liftIO $ hPutStrLn stderr $ "Error decoding perfdata (" ++ show line ++ "): " ++ show err
        Right unnormalisedDatum -> do
            let datum = if (optNormalise collectorOpts)
                        then convertPerfdataToBase unnormalisedDatum
                        else unnormalisedDatum    
            let (badPoints, goodPoints) = partitionPoints . unpackMetrics $ datum
            putDebugLn $ "Decoded datum: " ++ show datum ++ " - " ++ show (length goodPoints) ++ " valid metrics"
            liftIO $ mapM_ emitWarning badPoints
            mapM_ (uncurry (sendPoint collectorSpoolFiles (datumTimestamp datum))) goodPoints
            queueDatumSourceDict collectorSpoolFiles datum
  where
    sendPoint spool ts addr point = do
        putDebugLn $ "Writing datum " ++ show point ++ " with address " ++ show addr ++ " and timestamp " ++ show ts 
        liftIO $ queueSimple spool addr ts point
    datumTimestamp = TimeStamp . fromIntegral . perfdataTimestamp
    partitionPoints = partitionEithers . map shiftEither
    shiftEither :: (a, Either b c) -> Either b (a,c)
    shiftEither (x, y) = second (x,) y
    emitWarning err = hPutStrLn stderr $ "Warning: error decoding datapoint: " ++ err

-- | Read perfdata lines from stdin and queue them for writing to Vaultaire.
handleLines :: CollectorMonad ()
handleLines = do
    line <- liftIO $ try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ liftIO . hPutStrLn stderr $ "Error reading perfdata: " ++ show err
        Right l -> processLine l >> handleLines

main :: IO ()
main = parseOptions >>= flip runCollector (handleLines >> writeHashes)
