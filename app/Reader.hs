module Reader where

import Control.Concurrent (ThreadId, forkIO)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Control.Monad (forever)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, writeIORef)
import Database.SQLite.Simple (Connection)
import Storage.Types.Album (Album)
import Storage.Types.Artist (Artist)
import Storage.Types.Cache

type ReaderIO = ReaderT Env IO

data Env = Env
  { sqlConn :: Connection,
    cache :: Cache,
    cacheChannel :: IORef CacheChannel
  }

getSqlConnection :: ReaderIO Connection
getSqlConnection = do
  Env {..} <- ask
  return sqlConn

getCache :: ReaderIO Cache
getCache = do
  Env {..} <- ask
  return cache

getCacheChannel :: ReaderIO CacheChannel
getCacheChannel = do
  Env {..} <- ask
  lift $ readIORef cacheChannel

startCacheWorker :: IORef (CacheMap a) -> IO (CacheQueue a, ThreadId)
startCacheWorker cache = do
  chan@(_, outChan) <- Chan.newChan 1000
  threadId <- forkIO $ forever $ cacheWorker outChan cache
  return (chan, threadId)

cacheWorker :: Chan.OutChan (CacheQueueValue a) -> IORef (CacheMap a) -> IO ()
cacheWorker outChan cacheIORef = do
  CacheQueueValue primaryKey secondaryKeys foreignKey cacheValue <- Chan.readChan outChan
  cache <- readIORef cacheIORef
  case cacheValue of
    SecondaryIdx _ -> do
      -- TODO, update secondaryKeys list, don't replace it
      let newCache' = foldr (`HM.insert` PrimaryIdx foreignKey) cache (primaryKey : secondaryKeys)
          newCache = HM.insert foreignKey cacheValue newCache'
      writeIORef cacheIORef newCache
    _ -> pure ()

startArtistCacheWorker :: Cache -> IO (CacheQueue Artist, ThreadId)
startArtistCacheWorker cache = startCacheWorker (_artistCache cache)

startAlbumCacheWorker :: Cache -> IO (CacheQueue Album, ThreadId)
startAlbumCacheWorker cache = startCacheWorker (_albumCache cache)
