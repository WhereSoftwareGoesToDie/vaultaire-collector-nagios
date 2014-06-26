{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import qualified Data.ByteString as S
import System.IO
import System.IO.Error
import Control.Exception

import Data.Nagios.Perfdata
import Marquise.Client

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

handleLines :: CollectorOptions -> IO ()
handleLines op@CollectorOptions{..} = do
    line <- try S.getLine
    case line of
        Left err ->
            if isEOFError err
                then return ()
                else hPutStrLn stderr $ "Error reading perfdata: " ++ (show err)
        Right l -> do
            S.putStrLn l
            handleLines op

main :: IO ()
main = execParser collectorOptionParser >>= handleLines
