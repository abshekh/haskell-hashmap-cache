{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Storage.Cache.CacheWorker where

import Control.Concurrent (ThreadId, forkIO)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Control.Monad (forever)
import Data.Data (Proxy ())
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Database.Beam as B
import qualified Storage.FilterBy.Album as Album
import qualified Storage.FilterBy.Artist as Artist
import Storage.Types.Album (Album)
import Storage.Types.Artist (Artist)
import Storage.Types.Cache
import Storage.Types.CacheChannel (CacheQueue)
import Storage.Types.CacheClass (CacheClass (getAllKeys))

startCacheWorker ::
  ( B.Table a,
    Show (B.PrimaryKey a B.Identity),
    CacheClass (a B.Identity) filterBy
  ) =>
  IORef (CacheMap (a B.Identity)) ->
  Int ->
  IO (CacheQueue (a B.Identity) filterBy, ThreadId)
startCacheWorker cache maxQueueSize = do
  chan@(_, outChan) <- Chan.newChan maxQueueSize
  threadId <- forkIO $ forever $ cacheWorker outChan cache
  return (chan, threadId)

startArtistCacheWorker :: Cache -> IO (CacheQueue Artist Artist.FilterByOne, ThreadId)
startArtistCacheWorker cache = startCacheWorker (_artistCache cache) 1000

startAlbumCacheWorker :: Cache -> IO (CacheQueue Album Album.FilterByOneData, ThreadId)
startAlbumCacheWorker cache = startCacheWorker (_albumCache cache) 1000

cacheWorker ::
  ( B.Table a,
    Show (B.PrimaryKey a B.Identity),
    CacheClass (a B.Identity) filterBy
  ) =>
  Chan.OutChan (String, [a B.Identity], Proxy filterBy) ->
  IORef (CacheMap (a B.Identity)) ->
  IO ()
cacheWorker outChan cacheIORef = do
  (key, values, f) <- Chan.readChan outChan
  cache <- readIORef cacheIORef
  case values of
    [] -> return ()
    [value] -> do
      let allKeys = T.pack <$> key : getAllKeys value f
          foreignKey = T.pack . show $ B.primaryKey value
          newCache' = foldr (`HM.insert` PrimaryIdx foreignKey) cache allKeys
          -- TODO: don't to replace allKeys in ForeignIdx, make it a set and append to it
          newCache = HM.insert foreignKey (ForeignIdx (allKeys, value)) newCache'
      writeIORef cacheIORef newCache
    _ -> do
      let (newCache', foreignKeys) =
            foldr
              ( \v (c, fKeys) -> do
                  let (newC, foreignKey) = insertManyHelper c v f
                  (newC, foreignKey : fKeys)
              )
              (cache, [])
              values
          newCache = HM.insert (T.pack key) (PrimaryIdxs foreignKeys) newCache'
      writeIORef cacheIORef newCache
  where
    getAllKeysHelper value f = T.pack <$> getAllKeys value f
    getForeignKeyHelper value = T.pack . show $ B.primaryKey value
    insertManyHelper cache value f = do
      let allKeys = getAllKeysHelper value f
          foreignKey = getForeignKeyHelper value
          newCache' = foldr (`HM.insert` PrimaryIdx foreignKey) cache allKeys
          -- TODO: don't to replace allKeys in ForeignIdx, make it a set and append to it
          newCache = HM.insert foreignKey (ForeignIdx (allKeys, value)) newCache'
      (newCache, foreignKey)
