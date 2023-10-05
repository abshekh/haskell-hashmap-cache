{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Data (Proxy (Proxy))
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.HashMap.Strict as HM
import Data.IORef (newIORef, readIORef)
import Database.SQLite.Simple
import qualified Reader as R
import Storage.QueriesMiddleware.Album
import Storage.QueriesMiddleware.Artist
import Storage.Types.Artist
import Storage.Cache.Cache
import Storage.Cache.CacheTH
import Storage.Cache.CacheChannel
import Storage.Cache.CacheWorker

main :: IO ()
main = do
  conn <- open "resources/chinook.db"
  cache <- getDefaultCache (Proxy @Cache) (Proxy @CacheEnabled) (Proxy @CacheStrategy) (Proxy @CacheChannel)
  let cacheEnabled = CacheEnabled {
    _artistCache = True,
    _albumCache = False
  }
  let cacheStrategy = CacheStrategy {
    _artistCache = DefaultCache,
    _albumCache = LRUCache
  }
  cacheChannel' <- startCacheWorkers cache cacheEnabled cacheStrategy (Proxy @CacheChannel)

  -- (albumCacheChan, albumCacheThreadId) <- startAlbumCacheWorker cache
  -- (artistCacheChan, artistCacheThreadId) <- startArtistCacheWorker cache
  --
  -- let cacheChannel' =
  --       CacheChannel
  --         { _albumCache = albumCacheChan,
  --           _artistCache = artistCacheChan
  --         }
  cacheChannel <- newIORef cacheChannel'

  let runApp = runReaderT app :: R.Env -> IO ()
      env =
        R.Env
          { sqlConn = conn,
            cache = cache,
            cacheChannel = cacheChannel
          }
  runApp env
  -- killThread albumCacheThreadId
  -- killThread artistCacheThreadId

app :: R.ReaderIO ()
app = do
  artist <- selectOneArtistById 1
  lift $ print artist
  lift $ putStrLn ""

  artist <- selectOneArtistById 1
  lift $ print artist
  lift $ putStrLn ""

  void selectAllArtist

  lift $ threadDelay 5 -- microseconds

  artist <- selectOneArtistById 1
  lift $ print artist
  lift $ putStrLn ""

  artist <- selectOneArtistById 2
  lift $ print artist
  lift $ putStrLn ""

  artist <- selectOneArtistByName "AC/DC"
  lift $ print artist
  lift $ putStrLn ""

  void selectAllArtist
  -- showCache

-- showCache :: R.ReaderIO ()
-- showCache = do
--   cache <- R.getCache
--   artistCache <- lift $ readIORef $ _artistCache cache
--   albumCache <- lift $ readIORef $ _albumCache cache
--   lift $ putStrLn $ "artistCache: " ++ show artistCache
--   lift $ putStrLn $ "albumCache: " ++ show albumCache
