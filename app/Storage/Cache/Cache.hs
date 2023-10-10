{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Storage.Cache.Cache where

import Control.Lens
import Data.Data (Proxy)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef)
import Data.Text
import qualified Data.Time as DT
import Storage.Types.Album
import Storage.Types.Artist

data CacheValue a
  = ReferenceId (Maybe Text, DT.LocalTime)
  | ReferenceIds ([Text], DT.LocalTime)
  | Value a

type CacheMap a = HashMap Text (CacheValue a)

data CacheQueueValueData a = CacheQueueValueData
  { _primaryKey :: Text,
    _secondaryKeys :: [Text],
    _foriegnKeys :: Text,
    _cacheValue :: CacheValue a
  }

data Cache = Cache
  { _albumCache :: IORef (CacheMap Album),
    _artistCache :: IORef (CacheMap Artist)
  }
makeFieldsNoPrefix ''Cache

class CacheClass tableRecord filterBy where
  getAllKeys :: tableRecord -> Proxy filterBy -> [String]
  getKey :: Proxy tableRecord -> filterBy -> String

data CacheEnabled = CacheEnabled
  { _albumCache :: Bool,
    _artistCache :: Bool
  }
makeFieldsNoPrefix ''CacheEnabled

data CacheType = LRUCache | DefaultCacheWithEviction | DefaultCache

data CacheStrategy = CacheStrategy
  { _albumCache :: CacheType,
    _artistCache :: CacheType
  }
makeFieldsNoPrefix ''CacheStrategy

class CacheConfig cache cacheEnabled cacheStrategy cacheChannel where
  getDefaultCache :: Proxy cache -> Proxy cacheEnabled -> Proxy cacheStrategy -> Proxy cacheChannel -> IO cache
  startCacheWorkers :: cache -> cacheEnabled -> cacheStrategy -> Proxy cacheChannel -> IO cacheChannel
