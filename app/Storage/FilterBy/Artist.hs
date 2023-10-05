module Storage.FilterBy.Artist where

import Data.Text (Text)
import Data.Int (Int32)
import Storage.Types.Artist
import Storage.Cache.CacheTH

data FilterByOne
  = FilterByName {artistName :: Text}
  | FilterById {artistId :: Int32}
  deriving
    ( -- | FilterByArtistIdAndArtistName {artistId :: Int32, artistName :: Text}
      -- | FilterByArtistNameOrArtistNameL {artistName :: Text, artistNameL :: Text}
      Show
    )

$(deriveCacheClass ''Artist ''FilterByOne)
