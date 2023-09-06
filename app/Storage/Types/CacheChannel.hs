module Storage.Types.CacheChannel where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Storage.Types.Album
import qualified Storage.FilterBy.Album as Album
import Storage.Types.Artist
import Data.Data (Proxy)
import qualified Storage.FilterBy.Artist as Artist

-- import Storage.Types.Customer
-- import Storage.Types.Employee
-- import Storage.Types.Genre
-- import Storage.Types.Invoice
-- import Storage.Types.InvoiceLine
-- import Storage.Types.MediaType
-- import Storage.Types.Playlist
-- import Storage.Types.PlaylistTrack
-- import Storage.Types.Track

type CacheQueue a f = (Chan.InChan (String, [a], Proxy f), Chan.OutChan (String, [a], Proxy f))

data CacheChannel = CacheChannel
  { _albumCacheQueue :: CacheQueue Album Album.FilterByOneData,
    _artistCacheQueue :: CacheQueue Artist Artist.FilterByOne
  }
