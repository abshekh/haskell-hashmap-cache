module Reader where

import Control.Monad.Trans.Reader
import Database.SQLite.Simple (Connection)
import Control.Concurrent (MVar)
import Data.Text (Text)
import Data.HashMap.Internal
import Storage.Types.Cache (Cache)

type ReaderIO = ReaderT Env IO

data Env = Env
  { sqlConn :: Connection,
    cache :: MVar (HashMap Text Cache)
  }

getSqlConnection :: ReaderIO Connection
getSqlConnection = do
  Env {..} <- ask
  return sqlConn

getCache :: ReaderIO (MVar (HashMap Text Cache))
getCache = do
  Env {..} <- ask
  return cache
