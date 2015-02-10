-- | IO and State

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Vaultaire.Collector.Nagios.Perfdata.State where

import           Vaultaire.Collector.Nagios.Perfdata.Options
import           Vaultaire.Collector.Nagios.Perfdata.Process
import           Vaultaire.Collector.Nagios.Perfdata.Types

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Crypto.Cipher.AES
import qualified Data.ByteString                             as S
import qualified Network.Socket                              as N
import qualified Network.Socket.ByteString                   as NBS
import           System.IO.Error
import           System.Log.Logger

import           Data.Nagios.Perfdata
import           Data.Nagios.Perfdata.Metric
import           Vaultaire.Collector.Common.Process

import           Paths_vaultaire_collector_nagios            (version)

-- | Core functions

-- | Parses options off the command line, then runs the collector
-- | or gearman collector daemon accordingly
runNagios :: IO ()
runNagios = do
    infoM "State.runNagios" $ concat ["Collector version ", show version, " starting."]
    runCollectorN collectorOptions initialiseExtraState cleanup handleLines
  where
    initialiseExtraState (_, NagiosOptions{..}) = do
        conn <- if optTelemetry then do
            result <- connect optTelemetryHost optTelemetryPort
            case result of
                Left e -> do
                    warningM "State.runNagios" $ "Error setting up telemetry connection: " ++ show e
                    return Nothing
                Right success -> return $ Just success
                else return Nothing
        return $ NagiosState Nothing conn
--        (aesLog, aes) <- if optGearmanMode
--        then do
--            key <- loadKey optKeyFile
--            case key of
--                Left e ->
--                    return (logWarnN $ T.pack $ "Error loading key: " ++ show e, Nothing)
--                Right k -> return (return (), Just k)
--        else
--            return (return (), Nothing)

    connect :: String -> String -> IO (Either String Connection)
    connect host port = do
        sock <- N.socket N.AF_INET N.Stream 6 -- Create new ipv4 TCP socket.
        ai <- getHostAddress host port
        case ai of
            Nothing -> return $ Left $ "could not resolve address" ++ host
            Just x  -> do
                N.connect sock (N.addrAddress x)
                return $ Right $ Connection host port sock

    cleanup :: Nagios ()
    cleanup = do
        (_, NagiosState{..}) <- get
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

telemetryOut :: S.ByteString -> Nagios ()
telemetryOut output = do
    (_, NagiosState{..}) <- get
    case collectorTelemetryConn of
        Nothing -> return ()
        Just Connection{..} -> do
            let expected = S.length output
            sent <- liftIO $ NBS.send sock output
            when (sent /= expected) $ liftIO $ warningM "State.telemetryOut" $ concat
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
processLine :: S.ByteString -> Nagios ()
processLine line = do
    (_, NagiosOptions{..}) <- ask
    liftIO $ debugM "State.processLine" $ "Decoding line: " ++ show line
    case perfdataFromDefaultTemplate line of
        Left err                -> liftIO $ warningM "State.processLine" $ concat ["Error decoding perfdata (", show line, "): ", show err]
        Right unnormalisedDatum -> processDatum $
                                   if optNormalise then convertPerfdataToBase unnormalisedDatum
                                   else unnormalisedDatum

-- | Read perfdata lines from stdin and queue them for writing to Vaultaire.
handleLines :: Nagios ()
handleLines  = do
    (_, NagiosOptions{..}) <- ask
    line <- liftIO $ try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ liftIO $ errorM "State.handleLines" $ "Error reading perfdata: " ++ show err
        Right l -> do
            processLine l
            handleLines
