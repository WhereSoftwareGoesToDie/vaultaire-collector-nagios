-- | IO and State

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module State where

import Cache
import Options

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Word
import System.Directory
import System.IO

import Marquise.Client
-- Encapsulates the maintained state required by the collector
data CollectorState = CollectorState {
    collectorOpts :: CollectorOptions,
    collectorSpoolFiles :: SpoolFiles,
    collectorHashes :: IORef SourceDictCache,
    collectorHashFile :: FilePath
}

newtype CollectorMonad a = CollectorMonad (ReaderT CollectorState IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader CollectorState)

-- | Writes out the final state of the cache to the hash file    
writeHashes :: CollectorMonad ()
writeHashes = do
    collectorState <- ask
    cache <- liftIO $ readIORef $ collectorHashes collectorState
    let encodedCache = encodeCache cache
    let filePath = collectorHashFile collectorState
    liftIO $ L.writeFile filePath encodedCache

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
    fileExists <- doesFileExist cacheFile
    initialCache <- 
        if not fileExists
        then do
            putDebug $ "hash file doesn't exist"
            putDebug $ "Continuing with empty initial cache"
            return emptyCache
        else do
            putDebug $ "Opening" ++ cacheFile
            handle <- openFile cacheFile ReadMode
            putDebug "Reading contents"
            contents <- L.hGetContents handle
            let result = decodeCache contents
            result `seq` return ()
            putDebug "Got result"
            putDebug "Closing"
            hClose handle `seq` return ()
            putDebug "Closed"
            case result of
                Left e -> do
                    putDebug $ concat ["Error decoding hash filed: ", show e]
                    putDebug $ "Continuing with empty initial cache"
                    return emptyCache
                Right cache -> do
                    return cache
    newIORef initialCache
