-- | Gearman specific stuff

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Vaultaire.Collector.Nagios.Perfdata.Gearman where

import           Vaultaire.Collector.Nagios.Perfdata.Process
import           Vaultaire.Collector.Nagios.Perfdata.Types

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Cipher.AES
import qualified Data.ByteString                        as S
import qualified Data.ByteString.Base64                 as B64
import qualified Data.ByteString.Lazy.Char8             as L
import qualified Data.Text                              as T

import           Data.Nagios.Perfdata
import           System.Gearman.Connection
import           System.Gearman.Worker

gearmanProcessDatum :: Collector WorkerFunc
gearmanProcessDatum = do
    state@CollectorState{..} <- ask
    let CollectorOptions{..} = collectorOpts
    return $ \Job{..} -> flip runReaderT state $ unCollector $
        case clearBytes collectorAES jobData of
            Left e -> do
                logErrorN $ T.pack $ concat ["error decoding: ", show e, " data: ", show jobData]
                return $ Left . Just $ L.pack e
            Right checkResult -> do
                logDebugN $ T.pack $ "Null trimmed data: " ++ (show . trimNulls) checkResult
                case perfdataFromGearmanResult checkResult of
                    Left err -> do
                        logErrorN $ T.pack $ "Error parsing check result: " ++ err
                        return $ Left $ Just (L.pack err)
                    Right datum -> do
                        logDebugN $ T.pack $ "Got datum: " ++ show datum
                        processDatum datum
                        return $ Right "done"
  where
    clearBytes k d = decodeJob k $ L.toStrict d
    trimNulls :: S.ByteString -> S.ByteString
    trimNulls = S.reverse . S.dropWhile (0 ==) . S.reverse

-- | Decodes a job's data packet using Base 64
decodeJob :: Maybe AES -> S.ByteString -> Either String S.ByteString
decodeJob k d = case B64.decode d of
    Right d' -> Right $ maybeDecrypt k d'
    Left e   -> Left e

-- | Possible decrypts payload (based on whether key is given)
maybeDecrypt :: Maybe AES -> S.ByteString -> S.ByteString
maybeDecrypt aes ciphertext = case aes of
    Nothing -> ciphertext -- Nothing to do, we assume the input is already in cleartext.
    Just k -> decryptECB k ciphertext

-- | Sets up the gearman worker daemon and runs a work loop
setupGearman :: Collector ()
setupGearman = do
    CollectorState{..} <- ask
    workFunc <- gearmanProcessDatum
    let opts@CollectorOptions{..} = collectorOpts
    disconnectErrorBox <- liftIO newEmptyMVar
    replicateM_ optWorkerThreads $ setupConnection disconnectErrorBox workFunc opts
    forever $ do
        err <- liftIO $ takeMVar disconnectErrorBox
        logWarnN $ T.pack $ concat ["Worker thread disconnected from gearmanServer with: ", err, " starting new connection"]
        setupConnection disconnectErrorBox workFunc opts
  where
    setupConnection box workFunc CollectorOptions{..} = liftIO $ forkIO $ runGearman optGearmanHost optGearmanPort $ do
        err <- work [(L.pack optFunctionName, workFunc, Nothing)]
        liftIO $ putMVar box err
