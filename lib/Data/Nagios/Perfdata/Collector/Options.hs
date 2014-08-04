{-# LANGUAGE GADTs #-}

-- | Options/Parsing

module Data.Nagios.Perfdata.Collector.Options where

import Options.Applicative

-- Encapsulates the possible flags and switches for both collectors
data CollectorOptions = CollectorOptions {
    optNamespace     :: String,
    optCacheFile     :: FilePath,
    optDebug         :: Bool,
    optNormalise     :: Bool,
    optGearmanMode   :: Bool,
    optGearmanHost   :: String,
    optGearmanPort   :: String,
    optWorkerThreads :: Int,
    optFunctionName  :: String,
    optKeyFile       :: String
}

parseOptions :: IO CollectorOptions
parseOptions = execParser optionParser

-- | Parser which include all help info
optionParser :: ParserInfo CollectorOptions
optionParser =
    info (helper <*> collectorOptions)
    (fullDesc <>
        progDesc "Vaultaire collector for Nagios perfdata files, can run with mod_gearman" <>
        header "vaultaire-collector-nagios - writes datapoints from Nagios perfdata files to Vaultaire. Can run in daemon mode using the gearman protocol"
    )

-- | The parser for all options for nagios-perfdata
collectorOptions :: Parser CollectorOptions
collectorOptions = CollectorOptions
    <$> strOption
        (long "marquise-namespace"
         <> short 'n'
         <> value "perfdata"
         <> metavar "MARQUISE-NAMESPACE"
         <> help "Marquise namespace to write to. Must be unique on a host basis.")
    <*> strOption
        (long "cache-file"
         <> short 'c'
         <> value "/var/tmp/collector_hash_cache"
         <> metavar "CACHE-FILE"
         <> help "Location to read/write cached SourceDicts")
    <*> switch
        (long "debug"
         <> short 'd'
         <> help "Write debugging output")
    <*> switch
        (long "normalise-metrics"
         <> short 's'
         <> help "Normalise metrics to base SI units")
    <*> switch
        (long "gearman"
         <> short 'g'
         <> help "Run in gearman mode")
    <*> strOption
        (long "gearman-host"
         <> short 'h'
         <> value "localhost"
         <> metavar "GEARMANHOST"
         <> help "Hostname of Gearman server.")
    <*> strOption
        (long "gearman-port"
         <> short 'p'
         <> value "4730"
         <> metavar "GEARMANPORT"
         <> help "Port number Gearman server is listening on.")
    <*> option
        (long "workers"
         <> short 'w'
         <> metavar "WORKERS"
         <> value 2
         <> help "Number of worker threads to run.")
    <*> strOption
        (long "function-name"
         <> short 'f'
         <> value "check_results"
         <> metavar "FUNCTION-NAME"
         <> help "Name of function to register with Gearman server.")
    <*> strOption
        (long "key-file"
         <> short 'k'
         <> value ""
         <> metavar "KEY-FILE"
         <> help "File from which to read AES key to decrypt check results. If unspecified, results are assumed to be in cleartext.")
