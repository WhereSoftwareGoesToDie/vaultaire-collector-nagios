-- | IO and State

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Data.Nagios.Perfdata.Collector.State where

import Data.Nagios.Perfdata.Collector.Cache
import Data.Nagios.Perfdata.Collector.Options

import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as L
import Data.IORef
import System.IO

import Marquise.Client
-- Encapsulates the maintained state required by the collector
data CollectorState = CollectorState {
    collectorOpts       :: CommonOptions,
    collectorSpoolFiles :: SpoolFiles,
    collectorHashes     :: IORef SourceDictCache,
    collectorHashFile   :: FilePath
}

data GearmanState = GearmanState {

}

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

newtype GearmanCollectorMonad a = GearmanCollectorMonad (ReaderT GearmanState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader GearmanState)


-- | Writes out the final state of the cache to the hash file    
writeHashes :: CollectorMonad ()
writeHashes = do
    collectorState <- ask
    cache <- liftIO $ readIORef $ collectorHashes collectorState
    let encodedCache = encodeCache cache
    let filePath = collectorHashFile collectorState
    liftIO $ bracket (openFile filePath WriteMode) (hClose) (\h -> L.hPut h encodedCache)

-- | Convenience functions for debugging
maybePut :: Bool -> String -> IO ()
maybePut True s = putStrLn s
maybePut False _ = return ()

putDebugLn :: String -> CollectorMonad ()
putDebugLn s = do
    CollectorState{..} <- ask
    liftIO $ maybePut (optDebug collectorOpts) s

-- | Attempts to generate an initial cache of SourceDicts from the given file path
-- If the file does not exist, or is improperly formatted returns an empty cache
getInitialCache :: FilePath -> (String -> IO ()) -> IO (IORef SourceDictCache)
getInitialCache cacheFile putDebug = do
    initialCache <- bracket (openFile cacheFile ReadWriteMode) (hClose) readCache
    newIORef initialCache
  where
    readCache h = do
        putDebug "Reading contents"
        contents <- L.hGetContents h
        let result = decodeCache contents
        putDebug "Got result"
        case result of
            Left e -> do
                putDebug $ concat ["Error decoding hash filed: ", show e]
                putDebug $ "Continuing with empty initial cache"
                return emptyCache
            Right cache -> do
                return cache
