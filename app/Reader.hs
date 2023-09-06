module Reader where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader
import Data.IORef (IORef, readIORef)
import Database.SQLite.Simple (Connection)
import Storage.Types.Cache
import Storage.Types.CacheChannel

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
