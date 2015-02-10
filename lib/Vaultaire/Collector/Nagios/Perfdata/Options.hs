-- | Options/Parsing

module Vaultaire.Collector.Nagios.Perfdata.Options where

import           Vaultaire.Collector.Nagios.Perfdata.Types

import           Options.Applicative

parseOptions :: IO NagiosOptions
parseOptions = execParser optionParser

-- | Parser which include all help info
optionParser :: ParserInfo NagiosOptions
optionParser =
    info (helper <*> collectorOptions)
    (fullDesc <>
        progDesc "Vaultaire collector for Nagios perfdata files, can run with mod_gearman" <>
        header "vaultaire-collector-nagios - writes datapoints from Nagios perfdata files to Vaultaire. Can run in daemon mode using the gearman protocol"
    )

-- | The parser for all options for nagios-perfdata
collectorOptions :: Parser NagiosOptions
collectorOptions = NagiosOptions
    <$> switch
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
    <*> switch
        (long "telemetry"
         <> short 't'
         <> help "Run telemetry")
    <*> strOption
        (long "telemetry-host"
         <> value "127.0.0.1"
         <> metavar "TELEMETRYHOST"
         <> help "Host to send telemetry data to")
    <*> strOption
        (long "telemetry-port"
         <> value "9447"
         <> metavar "TELEMETRYPORT"
         <> help "Port to use for telemetry.")
