module Data.Nagios.Perfdata.Collector.Rep where

import Crypto.Cipher.AES
import Data.IORef
import Data.Set
import Data.Word

import Marquise.Client

type SourceDictCache = Set Word64

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

-- | Encapsulates the state required by both collectors
data CollectorState = CollectorState {
    collectorOpts         :: CollectorOptions,
    collectorAES          :: Maybe AES,
    collectorSpoolFiles   :: SpoolFiles,
    collectorHashes       :: IORef SourceDictCache,
    collectorHashFile     :: FilePath
}
