module Storage.QueriesMiddleware.Album where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Reader as R
import Storage.Queries.Album
import Storage.Types.Album

selectOneAlbumById :: Int32 -> R.ReaderIO (Maybe Album)
selectOneAlbumById id' = selectOneMaybeAlbumCache (FilterByAlbumId id')

selectOneAlbumByTitle :: Text -> R.ReaderIO (Maybe Album)
selectOneAlbumByTitle title = selectOneMaybeAlbumCache (FilterByAlbumTitle title)

selectManyAlbumByArtist :: Int32 -> R.ReaderIO [Album]
selectManyAlbumByArtist id' = selectManyAlbumCache (FilterByArtistId id')
