{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad
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

handleLines :: CollectorOptions -> IO ()
handleLines op@CollectorOptions{..} = do
    line <- try S.getLine
    case line of
        Left err ->
            unless (isEOFError err) $ hPutStrLn stderr $ "Error reading perfdata: " ++ show err
        Right l -> do
            C.putStrLn l
            handleLines op

main :: IO ()
main = execParser collectorOptionParser >>= handleLines
