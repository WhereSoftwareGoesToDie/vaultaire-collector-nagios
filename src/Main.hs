{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative

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

collector :: CollectorOptions -> IO ()
collector opts@CollectorOptions{..} = do
    error "nyi"

main :: IO ()
main = execParser collectorOptionParser >>= collector
