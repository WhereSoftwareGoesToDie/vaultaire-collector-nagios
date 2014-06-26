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


getMetricId :: Perfdata -> String -> S.ByteString
getMetricId datum metric = 
    let host = perfdataHostname datum in
    let service = S.unpack $ perfdataServiceDescription datum in
    "host:" +.+ C.pack host +.+ ",metric:" +.+ C.pack metric +.+ ",service:" +.+ S.pack service +.+ ","

unpackMetrics :: Perfdata -> [(Address,Word64)]
unpackMetrics datum = 
    map ((getAddress datum . fst) &&& (extractValueWord . snd)) (perfdataMetrics datum)
  where
    getAddress p = hashIdentifier . getMetricId p
    extractValueWord = either (const 0) id . extractValueWordEither
    extractValueWordEither = decode . encode . flip metricValueDefault 0.0

processLine :: ByteString -> CollectorMonad ()
processLine line = do
    CollectorState{..} <- ask
    liftIO $ putStrLn $ "Decoding line: " ++ show line
    liftIO $ case perfdataFromDefaultTemplate line of
        Left err -> hPutStrLn stderr $ "Error decoding perfdata (" ++ show line ++ "): " ++ show err
        Right datum -> do
            putStrLn "Decoded datum."
            mapM_ (uncurry (sendPoint collectorSpoolFiles (datumTimestamp datum))) (unpackMetrics datum)
  where
    sendPoint spool ts addr = queueSimple spool addr ts
    datumTimestamp = TimeStamp . fromIntegral . perfdataTimestamp

handleLines :: CollectorMonad ()
handleLines = do
    line <- liftIO $ try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ liftIO . hPutStrLn stderr $ "Error reading perfdata: " ++ show err
        Right l -> processLine l >> handleLines

main :: IO ()
main = execParser collectorOptionParser >>= flip runCollector handleLines
