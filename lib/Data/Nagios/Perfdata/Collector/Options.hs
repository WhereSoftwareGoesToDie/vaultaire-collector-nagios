-- | Options/Parsing

module Data.Nagios.Perfdata.Collector.Options where

import Options.Applicative

-- Encapsulates the possible flags and switches for the collector
data CollectorOptions = CollectorOptions {
    optNamespace :: String,
    optCacheFile :: FilePath,
    optDebug :: Bool,
    optNormalise :: Bool
}

parseOptions :: IO CollectorOptions
parseOptions = execParser collectorOptionParser

-- | The parser for all options for nagios-perfdata
opts :: Parser CollectorOptions
opts = CollectorOptions
    <$> strOption
        (long "marquise-namespace"
         <> short 'n'
         <> value "perfdata"
         <> metavar "MARQUISE-NAMESPACE"
         <> help "Marquise namespace to write to. Must be unique on a host basis.")
    <*> strOption
        (long "cache-file"
         <> short 'f'
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

-- | Parser which include all help info        
collectorOptionParser :: ParserInfo CollectorOptions
collectorOptionParser =
    info (helper <*> opts)
    (fullDesc <> 
        progDesc "Vaultaire collector for Nagios perfdata files" <>
        header "vaultaire-collector-nagios - writes datapoints from Nagios perfdata files to Vaultaire")
