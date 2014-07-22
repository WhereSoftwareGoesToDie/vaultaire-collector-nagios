-- This file is part of vaultaire-collector-nagios.
--
-- Copyright 2014 Anchor Systems Pty Ltd and others.
-- 
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under the terms of the BSD license.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Main where

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
import Data.Word
import Data.Serialize
import qualified Data.HashMap.Strict as HashMap(fromList)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Hashable
import Data.Bifunctor (second,bimap)
import qualified Data.Binary as B
import qualified  Data.Binary.Get as G
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
collectorVersion = "2.0.2"

(+.+) :: S.ByteString -> S.ByteString -> S.ByteString
(+.+) = S.append

data CollectorOptions = CollectorOptions {
    optNamespace :: String,
    optHashFile :: FilePath,
    optDebug :: Bool
}

data CollectorState = CollectorState {
    collectorOpts :: CollectorOptions,
    collectorSpoolFiles :: SpoolFiles,
    collectorHashes :: IORef (Set Int),
    collectorHashFile :: FilePath
}

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op@CollectorOptions{..} (CollectorMonad act) = do
    maybePut optDebug $ "Collector version " ++ collectorVersion ++ " starting."
    files <- createSpoolFiles optNamespace
    initialHashes <- getInitialHashes optHashFile (maybePut optDebug) 
    runReaderT act $ CollectorState op files initialHashes optHashFile

maybePut :: Bool -> String -> IO ()
maybePut True s = putStrLn s
maybePut False _ = return ()

getInitialHashes :: FilePath -> (String -> IO ()) -> IO (IORef (Set Int))
getInitialHashes hashFile putDebug = do
    hashList <- getHashList
    newIORef $ fromList hashList
      where
        getHashList :: IO [Int]
        getHashList = do
            fileExists <- doesFileExist hashFile
            case fileExists of
                True -> do
                    putDebug $ "Opening" ++ hashFile
                    handle <- openFile hashFile ReadMode
                    putDebug "Reading contents"
                    contents <- L.hGetContents handle
                    putDebug "Closing"
                    putDebug "Closed"
                    let result = G.runGetOrFail B.get contents
                    putDebug "Got result"
                    result `seq` hClose handle
                    case result of
                        Left (_, _, e) -> do
                            hPutStrLn stderr $ concat ["Error reading hash file: ", show e]
                            hPutStrLn stderr $ "Continuing with empty initial hashmap"
                            return []
                        Right (_, _, hashList) -> do
                            return hashList
                False -> return []

opts :: Parser CollectorOptions
opts = CollectorOptions
    <$> strOption
        (long "marquise-namespace"
         <> short 'n'
         <> value "perfdata"
         <> metavar "MARQUISE-NAMESPACE"
         <> help "Marquise namespace to write to. Must be unique on a host basis.")
    <*> strOption
        (long "hash-file"
         <> short 'f'
         <> value "/var/tmp/collector_hash_cache"
         <> metavar "HASH-FILE"
         <> help "Location to read/write cached SourceDicts")
    <*> switch
        (long "debug"
         <> short 'd'
         <> help "Write debugging output")


collectorOptionParser :: ParserInfo CollectorOptions
collectorOptionParser =
    info (helper <*> opts)
    (fullDesc <> 
        progDesc "Vaultaire collector for Nagios perfdata files" <>
        header "vaultaire-collector-nagios - writes datapoints from Nagios perfdata files to Vaultaire")

-- | Returns the Vaultaire SourceDict for the supplied metric in datum,
-- or an error if the relevant values have invalid characters (',' or
-- ':'). 
getSourceDict :: Perfdata -> String -> UOM -> Either String SourceDict
getSourceDict datum metric uom = 
    makeSourceDict . HashMap.fromList $ buildList datum metric uom

buildList :: Perfdata -> String -> UOM -> [(Text, Text)]    
buildList datum metric uom
    | isCounter uom = zip (map T.pack ["host", "metric", "service", "_float", "_counter"]) (map fmtTag  [host, metric, service, "1", "1"])
    | otherwise     = zip (map T.pack ["host", "metric", "service", "_float"]) (map fmtTag  [host, metric, service, "1"])
  where
    -- host, metric and service are collectively the primary key for
    -- this metric. As the nagios-perfdata package currently treats
    -- all values as floats, we also specify this as metadata for
    -- the presentation layer.
    host = perfdataHostname datum
    service = C.unpack $ perfdataServiceDescription datum
    isCounter Counter = True
    isCounter _       = False


hashList :: Perfdata -> String -> UOM -> Int
hashList datum metric uom = hash $ buildList datum metric uom

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
-- supplied perfdatum.
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
        
--        mapM_ (putStrLn . show) $ Prelude.filter (isJust) $ map (lookupSource (T.pack "service")) sds
  where
    getChanges :: Set Int -> (String, UOM) -> Maybe (Int, (Address, Either String SourceDict))
    getChanges hashes (metric, uom)
        | member currentHash hashes = Nothing
        | otherwise = changes
            where
                currentHash = hashList datum metric uom
                changes = Just (currentHash, (getAddress datum metric, getSourceDict datum metric uom))
    maybeUpdate addr sd =
        case sd of
            Left err -> hPutStrLn stderr $ "Error updating source dict: " ++ show err
            Right dict -> queueSourceDictUpdate spool addr dict

putDebugLn :: String -> CollectorMonad ()
putDebugLn s = do
    CollectorState{..} <- ask
    liftIO $ maybePut (optDebug collectorOpts) s
    return ()

-- | Given a line formatted according to the standard Nagios perfdata
-- template, a) queue it for writing to Vaultaire and b) queue an update
-- for each metric's metadata.
processLine :: ByteString -> CollectorMonad ()
processLine line = do
    CollectorState{..} <- ask
    putDebugLn $ "Decoding line: " ++ show line
    case perfdataFromDefaultTemplate line of
        Left err -> liftIO $ hPutStrLn stderr $ "Error decoding perfdata (" ++ show line ++ "): " ++ show err
        Right datum -> do
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

writeHashes :: CollectorMonad ()
writeHashes = do
    collectorState <- ask
    hashes <- liftIO $ readIORef $ collectorHashes collectorState
    let output = B.encode (toList hashes)
    let filePath = collectorHashFile collectorState
    liftIO $ L.writeFile filePath output

main :: IO ()
main = execParser collectorOptionParser >>= flip runCollector (handleLines >> writeHashes)
