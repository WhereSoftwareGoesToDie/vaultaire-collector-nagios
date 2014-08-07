-- | IO and State

{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE OverloadedStrings   #-}

module Data.Nagios.Perfdata.Collector.State where

import Data.Nagios.Perfdata.Collector.Cache
import Data.Nagios.Perfdata.Collector.Gearman(setupGearman)
import Data.Nagios.Perfdata.Collector.Options
import Data.Nagios.Perfdata.Collector.Process
import Data.Nagios.Perfdata.Collector.Rep
import Data.Nagios.Perfdata.Collector.Util

import Control.Exception
import Control.Monad.Reader
import Crypto.Cipher.AES
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.IORef
import System.IO
import System.IO.Error

import Data.Nagios.Perfdata
import Data.Nagios.Perfdata.Metric
import Marquise.Client

import Paths_vaultaire_collector_nagios (version)

-- | Core functions

-- | Parses options off the command line, then runs the collector
-- | or gearman collector daemon accordingly
runCollector :: IO ()    
runCollector = do 
    opts@CollectorOptions{..} <- parseOptions
    if optGearmanMode
    then runCollector' opts setupGearman
    else runCollector' opts handleLines
  where
    runCollector' :: CollectorOptions -> (CollectorState -> IO ()) -> IO ()
    runCollector' op@CollectorOptions{..} act = do
        maybePut optDebug $ "Collector version " ++ show version ++ " starting."
        files <- createSpoolFiles optNamespace
        hashes <- newIORef emptyCache
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
        let state = CollectorState op aes files hashes optCacheFile
        void $ getInitialCache state
        act state
        writeHashes state

-- | Writes out the final state of the cache to the hash file
writeHashes :: CollectorState -> IO ()
writeHashes CollectorState{..}= do
    cache <- readIORef collectorHashes
    let encodedCache = encodeCache cache
    liftIO $ bracket (openFile collectorHashFile WriteMode) (hClose) (\h -> L.hPut h encodedCache)

-- | Attempts to generate an initial cache of SourceDicts from the given file path
-- If the file does not exist, or is improperly formatted returns an empty cache
getInitialCache :: CollectorState -> IO ()
getInitialCache CollectorState{..} = do
    let CollectorOptions{..} = collectorOpts
    initialCache <- bracket (openFile collectorHashFile ReadWriteMode) (hClose) (readCache optDebug)
    writeIORef collectorHashes initialCache
  where
    readCache debug h = do
        (maybePut debug) "Reading cache file"
        contents <- L.hGetContents h
        let result = decodeCache contents
        (maybePut debug) "Decoding cache file"
        case result of
            Left e -> do
                (maybePut debug) $ concat ["Error decoding hash file: ", show e]
                (maybePut debug) $ "Continuing with empty initial cache"
                return emptyCache
            Right cache -> do
                return cache

-- | Loads the AES key from the given file path                
loadKey :: String -> IO (Either IOException AES)
loadKey fname = try $ S.readFile fname >>= return . initAES . trim 
  where
    trim = trim' . trim'
    trim' = S.reverse . S.dropWhile isBlank
    isBlank = flip elem (S.unpack " \n")

-- | Given a line formatted according to the standard Nagios perfdata
-- template, a) queue it for writing to Vaultaire and b) queue an update
-- for each metric's metadata.
processLine :: CollectorState -> S.ByteString -> IO ()
processLine state@CollectorState{..} line = do
    let CollectorOptions{..} = collectorOpts
    (maybePut optDebug) $ "Decoding line: " ++ show line
    parsedDatum <- case perfdataFromDefaultTemplate line of
        Left err -> do
            (maybePut optDebug) $ "Error decoding perfdata (" ++ show line ++ "): " ++ show err
            return Nothing
        Right unnormalisedDatum -> return $ Just $ if (optNormalise)
                                          then convertPerfdataToBase unnormalisedDatum
                                          else unnormalisedDatum
    case parsedDatum of
        Nothing -> return ()
        Just datum -> processDatum state datum

-- | Read perfdata lines from stdin and queue them for writing to Vaultaire.
handleLines :: CollectorState -> IO ()
handleLines state@CollectorState{..} = do
    let CollectorOptions{..} = collectorOpts
    line <- try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ (maybePut optDebug) $ "Error reading perfdata: " ++ show err
        Right l -> processLine state l >> handleLines state
