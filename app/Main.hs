module Main where

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
import Storage.Types.Cache
import Storage.Types.CacheChannel
import Storage.Cache.CacheWorker

main :: IO ()
main = do
  conn <- open "resources/chinook.db"
  cache <- getDefaultCache

  (albumCacheChan, albumCacheThreadId) <- startAlbumCacheWorker cache
  (artistCacheChan, artistCacheThreadId) <- startArtistCacheWorker cache

  let cacheChannel' =
        CacheChannel
          { _albumCacheQueue = albumCacheChan,
            _artistCacheQueue = artistCacheChan
          }
  cacheChannel <- newIORef cacheChannel'

  let runApp = runReaderT app :: R.Env -> IO ()
      env =
        R.Env
          { sqlConn = conn,
            cache = cache,
            cacheChannel = cacheChannel
          }
  runApp env
  killThread albumCacheThreadId
  killThread artistCacheThreadId

app :: R.ReaderIO ()
app = do
  artist <- selectOneArtistById 1
  lift $ print artist
  lift $ putStrLn ""

  lift $ threadDelay 5 -- microseconds

  artist <- selectOneArtistById 1
  lift $ print artist
  lift $ putStrLn ""

  artist <- selectOneArtistByName "AC/DC"
  lift $ print artist
  lift $ putStrLn ""

  -- artist <- selectOneArtistByNameAndNameL "AC/DC"
  -- lift $ print artist
  -- lift $ putStrLn ""
  --
  -- artist <- selectOneArtistByNameAndNameL "AC/DC"
  -- lift $ print artist
  -- lift $ putStrLn ""

  -- album <- selectManyAlbumByArtist 1
  -- lift $ print album
  -- lift $ putStrLn ""
  --
  -- album <- selectManyAlbumByArtist 1
  -- lift $ print album
  -- lift $ putStrLn ""
  --
  -- albums <- selectAllAlbum
  -- lift $ putStrLn ""

  -- album <- selectOneAlbumById 1
  -- lift $ print album
  -- lift $ putStrLn ""

  showCache

showCache :: R.ReaderIO ()
showCache = do
  cache <- R.getCache
  artistCache <- lift $ readIORef $ _artistCache cache
  albumCache <- lift $ readIORef $ _albumCache cache
  lift $ putStrLn $ "artistCache: " ++ show artistCache
  lift $ putStrLn $ "albumCache: " ++ show albumCache
