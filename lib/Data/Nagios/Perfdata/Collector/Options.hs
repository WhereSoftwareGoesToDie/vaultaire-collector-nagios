{-# LANGUAGE GADTs #-}

-- | Options/Parsing

module Data.Nagios.Perfdata.Collector.Options where

import Options.Applicative

data Mode a where
    NonGearman :: Mode CommonOptions
    Gearman    :: Mode GearmanOptions

-- Encapsulates the possible flags and switches for both collectors
data CommonOptions = CommonOptions {
    optNamespace :: String,
    optCacheFile :: FilePath,
    optDebug     :: Bool,
    optNormalise :: Bool
}

-- Encapsulates gearman specific options + common options
data GearmanOptions = GearmanOptions {
    gearOptGearmanHost   :: String,
    gearOptGearmanPort   :: String,
    gearOptWorkerThreads :: Int,
    gearOptFunctionName  :: String,
    gearOptKeyFile       :: String,
    gearOptCommonOptions :: CommonOptions
}

parseOptions :: Mode a -> IO a
parseOptions = execParser . optionParser

-- | The parser for all options for nagios-perfdata
commonOpts :: Parser CommonOptions
commonOpts = CommonOptions
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

gearmanOpts :: Parser GearmanOptions
gearmanOpts = GearmanOptions
    <$> strOption
        (long "gearman-host"
         <> short 'g'
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
    <*> commonOpts

-- | Parser which include all help info
optionParser :: Mode a -> ParserInfo a
optionParser NonGearman =
    info (helper <*> commonOpts)
    (fullDesc <>
        progDesc "Vaultaire collector for Nagios perfdata files" <>
        header "vaultaire-collector-nagios - writes datapoints from Nagios perfdata files to Vaultaire"
    )
optionParser Gearman =
    info (helper <*> gearmanOpts)
    (fullDesc <>
        progDesc "Vaultaire collector for Nagios with mod_gearman" <>
        header "vaultaire-collector-nagios-gearman - daemon to write Nagios perfdata to Vaultaire"
    )
