module Main where

import Control.Concurrent (forkIO, newMVar, putMVar, takeMVar, threadDelay, readMVar)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import qualified Data.HashMap.Strict as HM
import Database.SQLite.Simple (open)
import qualified Reader as R
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
  artist1 <- selectOneArtistById 1
  lift $ print artist1

  artist1' <- selectOneArtistById 1
  lift $ print artist1'

  artist1'' <- selectOneArtistByName "AC/DC"
  lift $ print artist1''

  -- artist2 <- selectOneArtistById 2
  -- lift $ print artist2
  --
  -- artist2' <- selectOneArtistById 2
  -- lift $ print artist2'
  --
  -- artist3 <- selectOneArtistByName "Aerosmith"
  -- lift $ print artist3
  --
  -- artist3' <- selectOneArtistByName "Aerosmith"
  -- lift $ print artist3'
  --
  -- artist4 <- selectOneArtistByName "Audioslave"
  -- lift $ print artist4

anotherApp :: R.ReaderIO ()
anotherApp = do
  env <- ask
  void $ lift $ forkIO $ do
    threadDelay (5 * 1000000) -- 10 sec
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
