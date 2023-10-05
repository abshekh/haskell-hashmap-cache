{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Cache.CacheWorker where

import Control.Monad
import Control.Concurrent (ThreadId, forkIO)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Control.Lens
import Data.Data (Proxy ())
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import qualified Data.Time as DT
import qualified Database.Beam as B
import Storage.Cache.Cache as C
import Storage.Cache.CacheChannel as CH
import Storage.Cache.CacheTH
import qualified Storage.FilterBy.Album as Album
import qualified Storage.FilterBy.Artist as Artist
import Storage.Types.Album (Album)
import Storage.Types.Artist (Artist)

startCacheWorker ::
  forall a filterBy.
  ( B.Table a,
    Show (B.PrimaryKey a B.Identity),
    CacheClass (a B.Identity) filterBy
  ) =>
  IORef (CacheMap (a B.Identity)) ->
  Bool ->
  CacheType ->
  Int ->
  Int -> -- make this maybe
  IO (Maybe (CacheQueue (a B.Identity) filterBy))
startCacheWorker cache enabled cacheStrategy maxQueueSize _maxLRUSize = do
  if enabled then do
    chan@(_, outChan) <- Chan.newChan maxQueueSize
    case cacheStrategy of
      DefaultCache -> do
        startDefaultCacheWorker' cache outChan
        return $ Just chan
      LRUCache -> do
        startLRUCacheWorker cache outChan _maxLRUSize
        return $ Just chan
    else return Nothing

startDefaultCacheWorker' ::
  (B.Table a, Show (B.PrimaryKey a B.Identity), CacheClass (a B.Identity) filterBy) =>
  IORef (CacheMap (a B.Identity)) ->
  Chan.OutChan (String, [a B.Identity], DT.LocalTime, Proxy filterBy) ->
  IO ()
startDefaultCacheWorker' cache outChan = do
  void $ forkIO $ forever $ cacheWorker outChan cache

startDefaultCacheWorker ::
  (B.Table a, Show (B.PrimaryKey a B.Identity), CacheClass (a B.Identity) filterBy) =>
  IORef (CacheMap (a B.Identity)) ->
  Int ->
  IO (CacheQueue (a B.Identity) filterBy, ThreadId)
startDefaultCacheWorker cache maxQueueSize = do
  chan@(_, outChan) <- Chan.newChan maxQueueSize
  threadId <- forkIO $ forever $ cacheWorker outChan cache
  return (chan, threadId)

startLRUCacheWorker ::
  ( B.Table a,
    Show (B.PrimaryKey a B.Identity),
    CacheClass (a B.Identity) filterBy
  ) =>
  IORef (CacheMap (a B.Identity)) ->
  Chan.OutChan (String, [a B.Identity], DT.LocalTime, Proxy filterBy) ->
  Int ->
  IO ()
startLRUCacheWorker cache outChan _maxLRUSize = do
  -- create an LRU of size maxLRUSize
  void $ forkIO $ forever $ cacheWorker outChan cache

startArtistCacheWorker :: Cache -> IO (CacheQueue Artist Artist.FilterByOne, ThreadId)
startArtistCacheWorker cache = startDefaultCacheWorker (cache ^. C.artistCache) 1000

startAlbumCacheWorker :: Cache -> IO (CacheQueue Album Album.FilterByOneData, ThreadId)
startAlbumCacheWorker cache = startDefaultCacheWorker (cache ^. C.albumCache) 1000

cacheWorker ::
  (B.Table a, Show (B.PrimaryKey a B.Identity), CacheClass (a B.Identity) filterBy) =>
  Chan.OutChan (String, [a B.Identity], DT.LocalTime, Proxy filterBy) ->
  IORef (CacheMap (a B.Identity)) ->
  IO ()
cacheWorker outChan cacheIORef = do
  (key, values, insertionTime, f) <- Chan.readChan outChan
  cache <- readIORef cacheIORef
  case values of
    [] -> do
      let newCache = HM.insert (T.pack key) (ReferenceId (Nothing, insertionTime)) cache
      writeIORef cacheIORef newCache
    [value] -> do
      let allKeys = T.pack <$> key : getAllKeys value f
          referenceId = T.pack . show $ B.primaryKey value
          newCache' = foldr (`HM.insert` ReferenceId (Just referenceId, insertionTime)) cache allKeys
          newCache = HM.insert referenceId (Value value) newCache'
      writeIORef cacheIORef newCache
    _ -> do
      let (newCache', referenceIds) =
            foldr
              ( \v (c, fKeys) -> do
                  let (newC, referenceId) = insertManyHelper c v f insertionTime
                  (newC, referenceId : fKeys)
              )
              (cache, [])
              values
          newCache = HM.insert (T.pack key) (ReferenceIds (referenceIds, insertionTime)) newCache'
      writeIORef cacheIORef newCache
  where
    getAllKeysHelper value f = T.pack <$> getAllKeys value f
    getReferenceIdHelper value = T.pack . show $ B.primaryKey value
    insertManyHelper cache value f insertionTime = do
      let allKeys = getAllKeysHelper value f
          referenceId = getReferenceIdHelper value
          newCache' = foldr (`HM.insert` ReferenceId (Just referenceId, insertionTime)) cache allKeys
          newCache = HM.insert referenceId (Value value) newCache'
      (newCache, referenceId)

lruCacheWorker ::
  (B.Table a, Show (B.PrimaryKey a B.Identity), CacheClass (a B.Identity) filterBy) =>
  Chan.OutChan (String, [a B.Identity], DT.LocalTime, Proxy filterBy) ->
  IORef (CacheMap (a B.Identity)) ->
  IO ()
lruCacheWorker outChan cacheIORef = do
  (key, values, insertionTime, f) <- Chan.readChan outChan
  cache <- readIORef cacheIORef
  case values of
    [] -> do
      let newCache = HM.insert (T.pack key) (ReferenceId (Nothing, insertionTime)) cache
      writeIORef cacheIORef newCache
    [value] -> do
      let allKeys = T.pack <$> key : getAllKeys value f
          referenceId = T.pack . show $ B.primaryKey value
          newCache' = foldr (`HM.insert` ReferenceId (Just referenceId, insertionTime)) cache allKeys
          newCache = HM.insert referenceId (Value value) newCache'
      writeIORef cacheIORef newCache
    _ -> do
      let (newCache', referenceIds) =
            foldr
              ( \v (c, fKeys) -> do
                  let (newC, referenceId) = insertManyHelper c v f insertionTime
                  (newC, referenceId : fKeys)
              )
              (cache, [])
              values
          newCache = HM.insert (T.pack key) (ReferenceIds (referenceIds, insertionTime)) newCache'
      writeIORef cacheIORef newCache
  where
    getAllKeysHelper value f = T.pack <$> getAllKeys value f
    getReferenceIdHelper value = T.pack . show $ B.primaryKey value
    insertManyHelper cache value f insertionTime = do
      let allKeys = getAllKeysHelper value f
          referenceId = getReferenceIdHelper value
          newCache' = foldr (`HM.insert` ReferenceId (Just referenceId, insertionTime)) cache allKeys
          newCache = HM.insert referenceId (Value value) newCache'
      (newCache, referenceId)

$(deriveCacheConfig ''Cache ''CacheEnabled ''CacheStrategy ''CacheChannel)
