module Storage.FilterBy.Album where

import Data.Text (Text)
import Data.Int (Int32)
import Storage.Types.Album
import Storage.Types.CacheTH

data FilterBy
  = FilterByOne FilterByOneData
  | FilterByMany FilterByManyData

data FilterByOneData
  = FilterByAlbumId {albumId :: Int32}
  | FilterByAlbumTitle {albumTitle :: Text}
  deriving (Show)

newtype FilterByManyData = FilterByArtistId {artistId :: Int32}
  deriving (Show)

$(deriveCacheClass ''Album ''FilterByOneData)

$(deriveCacheClass ''Album ''FilterByManyData)
