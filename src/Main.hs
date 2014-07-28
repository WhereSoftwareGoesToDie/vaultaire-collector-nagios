-- This file is part of vaultaire-collector-nagios.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Cache
import Options
import State

import Options.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import System.IO
import System.IO.Error
import System.Directory
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Arrow hiding (second)
import Crypto.MAC.SipHash
import Data.Word
import Data.Serialize
import qualified Data.HashMap.Strict as HashMap(fromList)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Bifunctor (second,bimap)
import qualified Data.Binary as B
import qualified Data.Binary.Get as G
import Data.Binary.IEEE754 (doubleToWord)
import Data.Maybe
import Data.Either (partitionEithers, rights)
import Data.Set hiding (map,partition)
import Prelude hiding (lookup)
import Data.IORef

import Data.Nagios.Perfdata
import Data.Nagios.Perfdata.Metric
import Marquise.Client
import Vaultaire.Types

collectorVersion :: String
collectorVersion = "2.1.2"

(+.+) :: S.ByteString -> S.ByteString -> S.ByteString
(+.+) = S.append

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op@CollectorOptions{..} (CollectorMonad act) = do
    maybePut optDebug $ "Collector version " ++ collectorVersion ++ " starting."
    files <- createSpoolFiles optNamespace
    initialHashes <- getInitialCache optCacheFile (maybePut optDebug) 
    runReaderT act $ CollectorState op files initialHashes optCacheFile

-- | Returns the Vaultaire SourceDict for the supplied metric in datum,
-- or an error if the relevant values have invalid characters (',' or
-- ':'). 
getSourceDict :: Perfdata -> String -> UOM -> Either String SourceDict
getSourceDict datum metric uom = 
    makeSourceDict . HashMap.fromList $ buildList datum metric uom

-- | Builds an association list for conversion into a SourceDict    
buildList :: Perfdata -> String -> UOM -> [(Text, Text)] 
buildList datum metric uom
    | (uom == Counter) = convert $ ("_counter", "1"):baseList
    | otherwise        = convert baseList
  where
    -- host, metric and service are collectively the primary key for
    -- this metric. As the nagios-perfdata package currently treats
    -- all values as floats, we also specify this as metadata for
    -- the presentation layer.
    host = perfdataHostname datum
    service = C.unpack $ perfdataServiceDescription datum
    baseList = zip ["host", "metric", "service", "_float", "_uom"] [host, metric, service, "1", show uom]
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

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum. Does not queue redundant updates
queueDatumSourceDict :: SpoolFiles -> Perfdata -> CollectorMonad ()
queueDatumSourceDict spool datum = do
    collectorState <- ask
    hashes <- liftIO $ readIORef $ collectorHashes collectorState
    let metrics = map (\(s, m) -> (s, metricUOM m)) $ perfdataMetrics datum
    let (hashUpdates, sdUpdates) = unzip $ mapMaybe (getChanges hashes) metrics
    let newHashes = fromList hashUpdates `union` hashes
    liftIO $ do
        writeIORef (collectorHashes collectorState) newHashes
        mapM_ (uncurry maybeUpdate) sdUpdates
        mapM_ (putStrLn . show) $ sdUpdates      
  where
    getChanges :: Set Word64 -> (String, UOM) -> Maybe (Word64, (Address, Either String SourceDict))
    getChanges hashes (metric, uom)
        | member currentHash hashes = Nothing
        | otherwise = changes
            where
                currentHash = hashList $ buildList datum metric uom
                changes = Just (currentHash, (getAddress datum metric, getSourceDict datum metric uom))
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
