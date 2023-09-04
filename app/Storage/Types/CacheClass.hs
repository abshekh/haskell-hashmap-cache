{-# LANGUAGE AllowAmbiguousTypes #-}

module Storage.Types.CacheClass where
import Data.Data (Proxy)

class CacheClass tableRecord filterBy where
  getAllKeys :: tableRecord -> Proxy filterBy -> [String]
  getKey :: Proxy tableRecord -> filterBy -> String
