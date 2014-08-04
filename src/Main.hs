-- This file is part of vaultaire-collector-nagios.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Nagios.Perfdata.Collector.Cache
import Data.Nagios.Perfdata.Collector.Options
import Data.Nagios.Perfdata.Collector.State
import Data.Nagios.Perfdata.Collector.Util

import Crypto.Cipher.AES
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Word
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64 as B64
import Data.Maybe
import Data.Either (partitionEithers)
import Data.Set hiding (map,partition)
import Prelude hiding (lookup)
import Data.IORef

import System.Gearman.Worker
import System.Gearman.Connection
import Data.Nagios.Perfdata
import Data.Nagios.Perfdata.Metric
import Marquise.Client

import Paths_vaultaire_collector_nagios (version)

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op@CollectorOptions{..} (CollectorMonad act) = do
    maybePut optDebug $ "Collector version " ++ show version ++ " starting."
    files <- createSpoolFiles optNamespace
    initialHashes <- getInitialCache optCacheFile (maybePut optDebug) 
    aes <- if optGearmanMode
    then do
        key <- loadKey optKeyFile
        case key of
            Left e -> do
                maybePut optDebug $ ("Error loading key: " ++ show e)
                return Nothing
            Right k -> return $ Just k
    else
        return Nothing
    runReaderT act $ CollectorState op aes files initialHashes optCacheFile

loadKey :: String -> IO (Either IOException AES)
loadKey fname = try $ S.readFile fname >>= return . initAES . trim 
  where
    trim = trim' . trim'
    trim' = S.reverse . S.dropWhile isBlank
    isBlank = flip elem (S.unpack " \n")

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum. Does not queue redundant updates
queueDatumSourceDict :: Bool -> SpoolFiles -> IORef SourceDictCache -> Bool -> Perfdata -> IO ()
queueDatumSourceDict debug spool hashesRef normalise datum = do
    hashes <- readIORef hashesRef
    let metrics = map (\(s, m) -> (s, metricUOM m)) $ perfdataMetrics datum
    let (hashUpdates, sdUpdates) = unzip $ mapMaybe (getChanges hashes) metrics
    let newHashes = fromList hashUpdates `union` hashes
    writeIORef hashesRef newHashes
    mapM_ (maybePut debug) (map (\x -> "Writing source dict: " ++ show x) sdUpdates)
    mapM_ (uncurry maybeUpdate) sdUpdates
  where
    getChanges :: Set Word64 -> (String, UOM) -> Maybe (Word64, (Address, Either String SourceDict))
    getChanges hashes (metric, uom)
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
 
processDatum :: Bool -> SpoolFiles -> IORef SourceDictCache -> Bool -> Perfdata -> IO ()
processDatum debug spool hashesRef normalise datum = do    
    let (badPoints, goodPoints) = partitionPoints . unpackMetrics $ datum
    maybePut debug $ concat ["Decoded datum: ", show datum, " - ", show (length goodPoints), " valid metrics"]
    mapM_ emitWarning badPoints
    mapM_ (uncurry (sendPoint (datumTimestamp datum))) goodPoints
    queueDatumSourceDict debug spool hashesRef normalise datum
  where
    partitionPoints = partitionEithers . map shiftEither
    shiftEither :: (a, Either b c) -> Either b (a,c)
    shiftEither (x, y) = second (x,) y
    emitWarning err = hPutStrLn stderr $ "Warning: error decoding datapoint: " ++ err
    sendPoint ts addr point = do
        maybePut debug $ concat["Writing datum ", show point, " with address ", show addr, " and timestamp ", show ts]
        queueSimple spool addr ts point
    datumTimestamp = TimeStamp . fromIntegral . perfdataTimestamp

processDatumM :: Perfdata -> CollectorMonad ()    
processDatumM datum = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    liftIO $ processDatum optDebug collectorSpoolFiles collectorHashes optNormalise datum

gearmanProcessDatum :: Bool -> Maybe AES -> SpoolFiles -> IORef SourceDictCache -> Bool -> WorkerFunc
gearmanProcessDatum debug key spool hashesRef normalise Job{..} = case (clearBytes key jobData) of
    Left e -> do
        maybePut debug ("error decoding: " ++ e)
        maybePut debug $ show jobData
        return . Left . Just $ L.pack e
    Right checkResult -> do
        ((maybePut debug) . (show . trimNulls)) checkResult
        case (perfdataFromGearmanResult checkResult) of
            Left err -> do 
                maybePut debug $ "Error parsing check result: " ++ err
                return $ Left $ Just (L.pack err)
            Right datum -> do
                maybePut debug $ "Got datum: " ++ (show datum)
                processDatum debug spool hashesRef normalise datum
                return $ Right "done"
  where
    clearBytes k d = decodeJob k $ L.toStrict d
    trimNulls :: S.ByteString -> S.ByteString
    trimNulls = S.reverse . (S.dropWhile ((0 ==))) . S.reverse

-- | Given a line formatted according to the standard Nagios perfdata
-- template, a) queue it for writing to Vaultaire and b) queue an update
-- for each metric's metadata.
processLine :: S.ByteString -> CollectorMonad ()
processLine line = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    putDebugLn $ "Decoding line: " ++ show line
    parsedDatum <- case perfdataFromDefaultTemplate line of
        Left err -> do
            putDebugLn $ "Error decoding perfdata (" ++ show line ++ "): " ++ show err
            return Nothing
        Right unnormalisedDatum -> return $ Just $ if (optNormalise)
                                          then convertPerfdataToBase unnormalisedDatum
                                          else unnormalisedDatum
    case parsedDatum of
        Nothing -> return ()
        Just datum -> processDatumM datum

decodeJob :: Maybe AES -> S.ByteString -> Either String S.ByteString
decodeJob k d = case (B64.decode d) of 
    Right d' -> Right $ maybeDecrypt k d'
    Left e   -> Left e 

maybeDecrypt :: Maybe AES -> S.ByteString -> S.ByteString
maybeDecrypt aes ciphertext = case aes of 
    Nothing -> ciphertext -- Nothing to do, we assume the input is already in cleartext.
    Just k -> decryptECB k ciphertext

setupGearman :: CollectorMonad ()
setupGearman = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    liftIO $ runGearman optGearmanHost optGearmanPort $ runWorker optWorkerThreads $ do
       void $ addFunc (L.pack optFunctionName) (gearmanProcessDatum optDebug collectorAES collectorSpoolFiles collectorHashes optNormalise) Nothing
       work
       return ()

-- | Read perfdata lines from stdin and queue them for writing to Vaultaire.
handleLines :: CollectorMonad ()
handleLines = do
    line <- liftIO $ try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ liftIO . hPutStrLn stderr $ "Error reading perfdata: " ++ show err
        Right l -> processLine l >> handleLines

main :: IO ()
main = do    
    opts@CollectorOptions{..} <- parseOptions
    if optGearmanMode
    then runCollector opts (setupGearman >> writeHashes)
    else runCollector opts (handleLines >> writeHashes)
