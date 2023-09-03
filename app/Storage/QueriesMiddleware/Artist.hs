module Storage.QueriesMiddleware.Artist where

import Data.Int (Int32)
import Data.Text (Text, toLower)
import qualified Reader as R
import Storage.Queries.Artist
import Storage.Types.Artist

selectOneArtistById :: Int32 -> R.ReaderIO (Maybe Artist)
selectOneArtistById id' = selectOneMaybeArtistCache (FilterById id')

selectOneArtistByName :: Text -> R.ReaderIO (Maybe Artist)
selectOneArtistByName name = selectOneMaybeArtistCache (FilterByName name)

selectOneArtistByNameAndNameL :: Text -> R.ReaderIO (Maybe Artist)
selectOneArtistByNameAndNameL name = selectOneMaybeArtistCache (FilterByArtistNameOrArtistNameL name (toLower name))
