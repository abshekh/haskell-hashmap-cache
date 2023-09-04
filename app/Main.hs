module Main where

import Control.Concurrent (forkIO, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import qualified Data.HashMap.Strict as HM
import Database.SQLite.Simple (open)
import qualified Reader as R
import Storage.QueriesMiddleware.Album
import Storage.QueriesMiddleware.Artist
import Storage.Types.Artist
import Storage.Types.Cache (Cache (ArtistCache))

main :: IO ()
main = do
  conn <- open "resources/chinook.db"
  cache <- newMVar mempty
  let runApp = runReaderT app :: R.Env -> IO ()
      -- runAnotherApp = runReaderT anotherApp :: R.Env -> IO ()
      env =
        R.Env
          { sqlConn = conn,
            cache = cache
          }
  -- runAnotherApp env
  runApp env

app :: R.ReaderIO ()
app = do
  artist <- selectOneArtistById 1
  lift $ print artist
  lift $ putStrLn ""

  artist <- selectOneArtistById 1
  lift $ print artist
  lift $ putStrLn ""

  -- artist <- selectOneArtistByName "AC/DC"
  -- lift $ print artist
  -- lift $ putStrLn ""

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

  cache <- showCache
  lift $ putStrLn $ "cache: " ++ cache

anotherApp :: R.ReaderIO ()
anotherApp = do
  env <- ask
  void $ lift $ forkIO $ do
    threadDelay (5 * 1000000) -- 5 sec
    c1 <- liftIO $ runReaderT showCache env
    putStrLn $ "thread1 cache: " ++ c1
  void $ lift $ forkIO $ do
    c2 <- liftIO $ runReaderT showCache env
    putStrLn $ "thread2 old cache: " ++ c2
    runReaderT mutateCache env
    c3 <- liftIO $ runReaderT showCache env
    putStrLn $ "thread2 new cache: " ++ c3

  c4 <- showCache
  lift $ putStrLn $ "main thread old cache: " ++ c4
  lift $ threadDelay (5 * 1000000) -- 10 sec
  c5 <- showCache
  lift $ putStrLn $ "main thread new cache: " ++ c5

showCache :: R.ReaderIO String
showCache = do
  cache <- R.getCache
  c <- lift $ readMVar cache
  return $ show c

mutateCache :: R.ReaderIO ()
mutateCache = do
  cache <- R.getCache
  _ <- lift $ takeMVar cache
  let artist = Artist 0 "unknown" :: Artist
      artistCache = ArtistCache artist
  let c = HM.fromList [("Artist-0", artistCache)]
  lift $ putMVar cache c
