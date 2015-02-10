module Vaultaire.Collector.Nagios.Perfdata.Types where

import           Crypto.Cipher.AES
import qualified Network.Socket                   as N

import           Vaultaire.Collector.Common.Types

-- Encapsulates the possible flags and switches for both collectors
data NagiosOptions = NagiosOptions {
    optNormalise     :: Bool,
    optGearmanMode   :: Bool,
    optGearmanHost   :: String,
    optGearmanPort   :: String,
    optFunctionName  :: String,
    optKeyFile       :: String,
    optTelemetry     :: Bool,
    optTelemetryHost :: String,
    optTelemetryPort :: String
}

-- | Encapsulates the state required by both collectors
data NagiosState = NagiosState {
    collectorAES           :: Maybe AES,
    collectorTelemetryConn :: Maybe Connection
}

data Connection = Connection {
    connHost :: String,
    connPort :: String,
    sock     :: N.Socket
}

type Nagios = Collector NagiosOptions NagiosState IO
