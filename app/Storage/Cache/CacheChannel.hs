{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Cache.CacheChannel where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Control.Lens
import Data.Data (Proxy)
import qualified Data.Time as DT
import qualified Storage.FilterBy.Album as Album
import qualified Storage.FilterBy.Artist as Artist
import Storage.Types.Album
import Storage.Types.Artist

type CacheQueue a f = (Chan.InChan (String, [a], DT.LocalTime, Proxy f), Chan.OutChan (String, [a], DT.LocalTime, Proxy f))

data CacheChannel = CacheChannel
  { _albumCache :: CacheQueue Album Album.FilterByOneData,
    _artistCache :: CacheQueue Artist Artist.FilterByOne
  }

makeFieldsNoPrefix ''CacheChannel
