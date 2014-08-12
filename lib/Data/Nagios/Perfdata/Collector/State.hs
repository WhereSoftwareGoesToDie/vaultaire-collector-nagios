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

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Crypto.Cipher.AES
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.IORef
import qualified Data.Text as T
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
    let logStartup = logInfoN $ T.pack $ concat ["Collector version ", show version, " starting."]
    action <- if optGearmanMode then
        return setupGearman
    else
        return handleLines
    runCollector' opts (logStartup >> getInitialCache >> action >> writeCache)
  where
    runCollector' :: CollectorOptions -> Collector () -> IO ()
    runCollector' op@CollectorOptions{..} (Collector act) = do
        hashes <- newIORef emptyCache
        files <- createSpoolFiles optNamespace
        (aesLog, aes) <- if optGearmanMode
        then do
            key <- loadKey optKeyFile
            case key of
                Left e ->
                    return (logWarnN $ T.pack $ "Error loading key: " ++ show e, Nothing)
                Right k -> return (return (), Just k)
        else
            return (return (), Nothing)
        let state = CollectorState op aes files hashes
        runReaderT (unCollector aesLog >> act) state

-- | Writes out the final state of the cache to the hash file
writeCache :: Collector ()
writeCache = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    cache <- liftIO $ readIORef collectorHashes
    let encodedCache = encodeCache cache
    liftIO $ bracket (openFile optCacheFile WriteMode) (hClose) (\h -> L.hPut h encodedCache)

-- | Attempts to generate an initial cache of SourceDicts from the given file path
-- If the file does not exist, or is improperly formatted returns an empty cache
getInitialCache :: Collector ()
getInitialCache = do
    state@CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    let setup = openFile optCacheFile ReadWriteMode
    let teardown = hClose
    let action = (\h -> runReaderT (unCollector $ readCache h) state)
    initialCache <- liftIO $ bracket setup teardown action
    liftIO $ writeIORef collectorHashes initialCache
  where
    readCache :: Handle -> Collector SourceDictCache
    readCache h = do
        CollectorState{..} <- ask
        logDebugN $ T.pack "Reading cache file"
        contents <- liftIO $ L.hGetContents h
        let result = decodeCache contents
        logDebugN $ T.pack "Decoding cache file"
        case result of
            Left e -> do
                logWarnN $ T.pack $ concat ["Error decoding hash file: ", show e, " Continuing with empty initial cache"]
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
processLine :: S.ByteString -> Collector ()
processLine line = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    logDebugN $ T.pack $ "Decoding line: " ++ show line
    parsedDatum <- case perfdataFromDefaultTemplate line of
        Left err -> do
            logErrorN $ T.pack $ concat ["Error decoding perfdata (", show line, "): ", show err]
            return Nothing
        Right unnormalisedDatum -> return $ Just $ if (optNormalise)
                                          then convertPerfdataToBase unnormalisedDatum
                                          else unnormalisedDatum
    case parsedDatum of
        Nothing -> return ()
        Just datum -> processDatum datum

-- | Read perfdata lines from stdin and queue them for writing to Vaultaire.
handleLines :: Collector ()
handleLines  = do
    CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    line <- liftIO $ try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ logErrorN $ T.pack $ "Error reading perfdata: " ++ show err
        Right l -> do
            processLine l
            handleLines
