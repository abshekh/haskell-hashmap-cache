module Storage.QueriesMiddleware.Album where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Reader as R
import Storage.Queries.Album
import Storage.Types.Album
import Storage.FilterBy.Album

selectOneAlbumById :: Int32 -> R.ReaderIO (Maybe Album)
selectOneAlbumById id' = selectOneMaybeAlbum (FilterByAlbumId id')

selectOneAlbumByTitle :: Text -> R.ReaderIO (Maybe Album)
selectOneAlbumByTitle title = selectOneMaybeAlbum (FilterByAlbumTitle title)

selectManyAlbumByArtist :: Int32 -> R.ReaderIO [Album]
selectManyAlbumByArtist id' = selectManyAlbum (FilterByArtistId id')

-- selectAllAlbum :: R.ReaderIO [Album]
-- selectAllAlbum = selectAllAlbum
