-- | IO and State

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Data.Nagios.Perfdata.Collector.State where

import           Data.Nagios.Perfdata.Collector.Gearman (setupGearman)
import           Data.Nagios.Perfdata.Collector.Options
import           Data.Nagios.Perfdata.Collector.Process
import           Data.Nagios.Perfdata.Collector.Rep

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Cipher.AES
import qualified Data.ByteString                        as S
import qualified Data.Text                              as T
import qualified Network.Socket                         as N
import qualified Network.Socket.ByteString              as NBS
import           System.IO.Error

import           Data.Nagios.Perfdata
import           Data.Nagios.Perfdata.Metric
import           Marquise.Client

import           Paths_vaultaire_collector_nagios       (version)

-- | Core functions

-- | Parses options off the command line, then runs the collector
-- | or gearman collector daemon accordingly
runCollector :: IO ()
runCollector = do
    opts@CollectorOptions{..} <- parseOptions
    let logStartup = logInfoN $ T.pack $ concat ["Collector version ", show version, " starting."]
    let action
          | optGearmanMode = setupGearman
          | otherwise = handleLines
    runCollector' opts (logStartup >> action)
  where
    runCollector' :: CollectorOptions -> Collector () -> IO ()
    runCollector' op@CollectorOptions{..} (Collector act) = do
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
        (connLog, conn) <- if optTelemetry
        then do
            result <- connect optTelemetryHost optTelemetryPort
            case result of
                Left e ->
                    return (logWarnN $ T.pack $ "Error setting up telemetry connection: " ++ show e, Nothing)
                Right success -> return (return (), Just success)
        else
            return (return (), Nothing)
        let state = CollectorState op aes files conn
        let (Collector setup)    = aesLog >> connLog
        let (Collector teardown) = cleanup
        runReaderT (setup >> act >> teardown) state

    connect :: String -> String -> IO (Either String Connection)
    connect host port = do
        sock <- N.socket N.AF_INET N.Stream 6 -- Create new ipv4 TCP socket.
        ai <- getHostAddress host port
        case ai of
            Nothing -> return $ Left $ "could not resolve address" ++ host
            Just x  -> do
                N.connect sock (N.addrAddress x)
                return $ Right $ Connection host port sock

    cleanup :: Collector ()
    cleanup = do
        CollectorState{..} <- ask
        case collectorTelemetryConn of
            Nothing -> return ()
            Just Connection{..} -> liftIO $ N.sClose sock

    getHostAddress :: String -> String -> IO (Maybe N.AddrInfo)
    getHostAddress host port = do
        ai <- N.getAddrInfo hints (Just host) (Just port)
        case ai of
            (x:_) -> return $ Just x
            [] -> return Nothing
      where
        hints = Just $ N.defaultHints {
            N.addrProtocol = 6,
            N.addrFamily   = N.AF_INET,
            N.addrFlags    = [ N.AI_NUMERICSERV ]
        }

telemetryOut :: S.ByteString -> Collector ()
telemetryOut output = do
    CollectorState{..} <- ask
    case collectorTelemetryConn of
        Nothing -> return ()
        Just Connection{..} -> do
            let expected = S.length output
            sent <- liftIO $ NBS.send sock output
            when (sent /= expected) $ logWarnN $ T.pack $ concat
                ["Telemetry send failed: only sent ", show sent, " bytes out of ", show expected]

-- | Loads the AES key from the given file path
loadKey :: String -> IO (Either IOException AES)
loadKey fname = try $ liftM (initAES . trim) (S.readFile fname)
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
    case perfdataFromDefaultTemplate line of
        Left err                -> logWarnN $ T.pack $ concat ["Error decoding perfdata (", show line, "): ", show err]
        Right unnormalisedDatum -> processDatum $
                                   if optNormalise then convertPerfdataToBase unnormalisedDatum
                                   else unnormalisedDatum

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
