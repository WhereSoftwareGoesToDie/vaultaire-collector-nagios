{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.Nagios.Perfdata.Collector.Rep where

import Crypto.Cipher.AES
import qualified Data.ByteString as S
import Data.IORef
import Data.Monoid
import Data.Set hiding (map)
import Data.Word
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import System.Log.FastLogger

import Marquise.Client
import Vaultaire.Types

type SourceDictCache = Set Word64

-- Encapsulates the possible flags and switches for both collectors
data CollectorOptions = CollectorOptions {
    optNamespace     :: String,
    optCacheFile     :: FilePath,
    optLogFile       :: FilePath,
    optLogLevel      :: LogLevel,
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
    collectorHashes       :: IORef SourceDictCache
}

newtype Collector a = Collector {
    unCollector :: (ReaderT CollectorState IO a)
} deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

instance MonadLogger Collector where
    monadLoggerLog _ _ level msg = do 
        CollectorState{..} <- ask
        let CollectorOptions{..} = collectorOpts
        if (level >= optLogLevel) then liftIO $ do
            currTime <- getCurrentTimeNanoseconds
            let logPrefix = mconcat $ map toLogStr [showLevel level, " ",  show currTime, " "]
            let logMsg = logPrefix <> (toLogStr msg) <> (toLogStr "\n")
            (S.appendFile optLogFile) $ fromLogStr logMsg
        else return ()
      where
        showLevel LevelDebug     = "[Debug]"
        showLevel LevelInfo      = "[Info]"
        showLevel LevelWarn      = "[Warning]"
        showLevel LevelError     = "[Error]"
        showLevel (LevelOther l) = concat ["[", show l, "]"]    
