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
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Arrow
import Data.Word
import Data.Serialize
import Data.HashMap.Strict (HashMap, fromList)
import qualified Data.Text as T
import Data.Text (Text)

import Data.Nagios.Perfdata
import Marquise.Client

(+.+) :: S.ByteString -> S.ByteString -> S.ByteString
(+.+) = S.append

data CollectorOptions = CollectorOptions {
    optNamespace :: String
}

data CollectorState = CollectorState {
    collectorOpts :: CollectorOptions,
    collectorSpoolFiles :: SpoolFiles
}

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

runCollector :: CollectorOptions -> CollectorMonad a -> IO a
runCollector op@CollectorOptions{..} (CollectorMonad act) = do
    files <- createSpoolFiles optNamespace
    runReaderT act $ CollectorState op files

opts :: Parser CollectorOptions
opts = CollectorOptions
    <$> strOption
        (long "marquise-namespace"
         <> short 'n'
         <> value "perfdata"
         <> metavar "MARQUISE-NAMESPACE"
         <> help "Marquise namespace to write to. Must be unique on a host basis.")

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
  where
    buildList datum metric = 
        let host = perfdataHostname datum in
        let service = C.unpack $ perfdataServiceDescription datum in
        -- host, metric and service are collectively the primary key for
        -- this metric. As the nagios-perfdata package currently treats
        -- all values as floats, we also specify this as metadata for
        -- the presentation layer.
        zip (map T.pack ["host", "metric", "service", "_float"]) (map fmtTag  [host, metric, service, "1"])
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
queueDatumSourceDict :: SpoolFiles -> Perfdata -> IO ()
queueDatumSourceDict spool datum = do
    let metrics = map fst $ perfdataMetrics datum
    mapM_ (uncurry maybeUpdate) $ zip (map (getAddress datum) metrics) (map (getSourceDict datum) metrics)
  where
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
    liftIO $ case perfdataFromDefaultTemplate line of
        Left err -> hPutStrLn stderr $ "Error decoding perfdata (" ++ show line ++ "): " ++ show err
        Right datum -> do
            putStrLn "Decoded datum."
            mapM_ (uncurry (sendPoint collectorSpoolFiles (datumTimestamp datum))) (unpackMetrics datum)
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
