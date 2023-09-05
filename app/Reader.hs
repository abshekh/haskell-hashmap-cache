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

startArtistCacheWorker :: Cache -> IO (CacheQueue Artist, ThreadId)
startArtistCacheWorker cache = do
  chan@(_, outChan) <- Chan.newChan 1000
  threadId <- forkIO $ forever $ artistCacheWorker outChan cache
  return (chan, threadId)

artistCacheWorker :: Chan.OutChan (CacheQueueValue Artist) -> Cache -> IO ()
artistCacheWorker outChan cache = do
  CacheQueueValue primaryKey secondaryKeys foreignKey cacheValue <- Chan.readChan outChan
  artistCache <- readIORef (_artistCache cache)
  case cacheValue of
    SecondaryIdx _ -> do
      -- TODO, update secondaryKeys list, don't replace it
      let newArtistCache' = foldr (`HM.insert` PrimaryIdx foreignKey) artistCache (primaryKey : secondaryKeys)
          newArtistCache = HM.insert foreignKey cacheValue newArtistCache'
      writeIORef (_artistCache cache) newArtistCache
    _ -> pure ()

startAlbumCacheWorker :: Cache -> IO (CacheQueue Album, ThreadId)
startAlbumCacheWorker cache = do
  chan@(_, outChan) <- Chan.newChan 1000
  threadId <- forkIO $ forever $ albumCacheWorker outChan cache
  return (chan, threadId)

albumCacheWorker :: Chan.OutChan (CacheQueueValue Album) -> Cache -> IO ()
albumCacheWorker outChan cache = do
  CacheQueueValue primaryKey secondaryKeys foreignKey cacheValue <- Chan.readChan outChan
  albumCache <- readIORef (_albumCache cache)
  case cacheValue of
    SecondaryIdx _ -> do
      let newAlbumCache' = foldr (`HM.insert` PrimaryIdx foreignKey) albumCache (primaryKey : secondaryKeys)
          newAlbumCache = HM.insert foreignKey cacheValue newAlbumCache'
      writeIORef (_albumCache cache) newAlbumCache
    _ -> pure ()
