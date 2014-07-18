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
import Control.Arrow
import Data.Word
import Data.Serialize
import Data.HashMap.Strict (lookup, HashMap, fromList, toList, union)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Hashable
import qualified Data.Binary as B
import qualified  Data.Binary.Get as G
import Data.Maybe
import Prelude hiding(lookup)
import Data.IORef

import Data.Nagios.Perfdata
import Marquise.Client
import Vaultaire.Types

(+.+) :: S.ByteString -> S.ByteString -> S.ByteString
(+.+) = S.append

data CollectorOptions = CollectorOptions {
    optNamespace :: String,
    optHashFile :: FilePath
}

data CollectorState = CollectorState {
    collectorOpts :: CollectorOptions,
    collectorSpoolFiles :: SpoolFiles,
    collectorHashes :: IORef (HashMap String Int),
    collectorHashFile :: FilePath
}

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op@CollectorOptions{..} (CollectorMonad act) = do
    files <- createSpoolFiles optNamespace
    initialHashes <- getInitialHashes optHashFile
    runReaderT act $ CollectorState op files initialHashes optHashFile

getInitialHashes :: FilePath -> IO (IORef (HashMap String Int))
getInitialHashes hashFile = do
    hashList <- getHashList
    newIORef $ fromList hashList
      where
        getHashList :: IO [(String, Int)]
        getHashList = do
            fileExists <- doesFileExist hashFile
            case fileExists of
                True -> do
                    contents <- L.readFile hashFile
                    let result = G.runGetOrFail B.get contents
                    case result of
                        Left (_, _, e) -> do
                            hPutStrLn stderr $ concat ["Error reading hash file: ", show e]
                            hPutStrLn stderr $ "Continuing with empty initial hashmap"
                            return []
                        Right (_, _, hashList) -> return hashList
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

collectorOptionParser :: ParserInfo CollectorOptions
collectorOptionParser =
    info (helper <*> opts)
    (fullDesc <> 
        progDesc "Vaultaire collector for Nagios perfdata files" <>
        header "vaultaire-collector-nagios - writes datapoints from Nagios perfdata files to Vaultaire")

-- | Returns the Vaultaire SourceDict for the supplied metric in datum,
-- or an error if the relevant values have invalid characters (',' or
-- ':'). 
getSourceDict :: Perfdata -> String -> Either String SourceDict
getSourceDict datum metric = 
    makeSourceDict . fromList $ buildList datum metric

buildList :: Perfdata -> String -> [(Text, Text)]    
buildList datum metric = 
    let host = perfdataHostname datum in
    let service = C.unpack $ perfdataServiceDescription datum in
    -- host, metric and service are collectively the primary key for
    -- this metric. As the nagios-perfdata package currently treats
    -- all values as floats, we also specify this as metadata for
    -- the presentation layer.
    zip (map T.pack ["host", "metric", "service", "_float"]) (map fmtTag  [host, metric, service, "1"])

hashList :: Perfdata -> String -> Int
hashList datum metric = hash $ buildList datum metric

fmtTag = T.pack . (map ensureValid)

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
unpackMetrics :: Perfdata -> [(Address,Word64)]
unpackMetrics datum = 
    map ((getAddress datum . fst) &&& (extractValueWord . snd)) (perfdataMetrics datum)
  where
    extractValueWord = either (const 0) id . extractValueWordEither
    extractValueWordEither = decode . encode . flip metricValueDefault 0.0

-- | Queue updates to the metadata associated with each metric in the
-- supplied perfdatum.
queueDatumSourceDict :: SpoolFiles -> Perfdata -> CollectorMonad ()
queueDatumSourceDict spool datum = do
    collectorState <- ask
    hashes <- liftIO $ readIORef $ collectorHashes collectorState
    let metrics = map fst $ perfdataMetrics datum
    let (hashChanges, updates) = unzip $ mapMaybe (getChanges hashes) metrics
    let newHashmap = union (fromList hashChanges) hashes
    liftIO $ writeIORef (collectorHashes collectorState) newHashmap
    liftIO $ mapM_ (uncurry maybeUpdate) updates
    liftIO $ B.encodeFile (collectorHashFile collectorState) (toList newHashmap)
  where
    getChanges :: (HashMap String Int) -> String -> Maybe ((String, Int), (Address, Either String SourceDict))
    getChanges hashes metric
        | isNothing oldHash = changes
        | fromJust oldHash == currentHash = Nothing
        | otherwise = changes
            where
                oldHash = lookup metric hashes
                currentHash = hashList datum metric
                changes = Just ((metric, currentHash), (getAddress datum metric, getSourceDict datum metric))
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
    liftIO $ putStrLn $ "Decoding line: " ++ show line
    case perfdataFromDefaultTemplate line of
        Left err -> liftIO $ hPutStrLn stderr $ "Error decoding perfdata (" ++ show line ++ "): " ++ show err
        Right datum -> do
            liftIO $ putStrLn "Decoded datum."
            liftIO $ mapM_ (uncurry (sendPoint collectorSpoolFiles (datumTimestamp datum))) (unpackMetrics datum)
            queueDatumSourceDict collectorSpoolFiles datum
  where
    sendPoint spool ts addr = queueSimple spool addr ts
    datumTimestamp = TimeStamp . fromIntegral . perfdataTimestamp

-- | Read perfdata lines from stdin and queue them for writing to Vaultaire.
handleLines :: CollectorMonad ()
handleLines = do
    line <- liftIO $ try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ liftIO . hPutStrLn stderr $ "Error reading perfdata: " ++ show err
        Right l -> processLine l >> handleLines

main :: IO ()
main = execParser collectorOptionParser >>= flip runCollector handleLines
