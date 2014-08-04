-- | Gearman specific stuff

{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Data.Nagios.Perfdata.Collector.Gearman where

import Data.Nagios.Perfdata.Collector.Process
import Data.Nagios.Perfdata.Collector.Rep
import Data.Nagios.Perfdata.Collector.Util

import Crypto.Cipher.AES
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as L 
import qualified Data.ByteString as S

import System.Gearman.Worker
import System.Gearman.Connection
import Data.Nagios.Perfdata

-- | Parses job then passes decoded datum to processDatum
-- | If any stage fails, returns exception as required
gearmanProcessDatum :: CollectorState -> WorkerFunc
gearmanProcessDatum state@CollectorState{..} Job{..} =
    let CollectorOptions{..} = collectorOpts in
    case (clearBytes collectorAES jobData) of
    Left e -> do
        maybePut optDebug ("error decoding: " ++ e)
        maybePut optDebug $ show jobData
        return . Left . Just $ L.pack e
    Right checkResult -> do
        ((maybePut optDebug) . (show . trimNulls)) checkResult
        case (perfdataFromGearmanResult checkResult) of
            Left err -> do 
                maybePut optDebug $ "Error parsing check result: " ++ err
                return $ Left $ Just (L.pack err)
            Right datum -> do
                maybePut optDebug $ "Got datum: " ++ (show datum)
                processDatum state datum
                return $ Right "done"
  where
    clearBytes k d = decodeJob k $ L.toStrict d
    trimNulls :: S.ByteString -> S.ByteString
    trimNulls = S.reverse . (S.dropWhile ((0 ==))) . S.reverse

-- | Decodes a job's data packet using Base 64
decodeJob :: Maybe AES -> S.ByteString -> Either String S.ByteString
decodeJob k d = case (B64.decode d) of 
    Right d' -> Right $ maybeDecrypt k d'
    Left e   -> Left e 

-- | Possible decrypts payload (based on whether key is given)     
maybeDecrypt :: Maybe AES -> S.ByteString -> S.ByteString
maybeDecrypt aes ciphertext = case aes of 
    Nothing -> ciphertext -- Nothing to do, we assume the input is already in cleartext.
    Just k -> decryptECB k ciphertext

-- | Sets up the gearman worker daemon and runs a work loop
setupGearman :: CollectorState -> IO ()
setupGearman state@CollectorState{..} = do
    let CollectorOptions{..} = collectorOpts
    runGearman optGearmanHost optGearmanPort $ runWorker optWorkerThreads $ do
       _ <- addFunc (L.pack optFunctionName) (gearmanProcessDatum state) Nothing
       work
       return ()
