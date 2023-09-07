module Storage.QueriesMiddleware.Artist where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Reader as R
import Storage.Queries.Artist
import Storage.Types.Artist
import Storage.FilterBy.Artist

selectOneArtistById :: Int32 -> R.ReaderIO (Maybe Artist)
selectOneArtistById id' = selectOneMaybeArtistCache (FilterById id')

-- selectOneArtistByIdAndName :: Int32 -> Text -> R.ReaderIO (Maybe Artist)
-- selectOneArtistByIdAndName id' name = selectOneMaybeArtistCache (FilterByArtistIdAndArtistName id' name)

selectOneArtistByName :: Text -> R.ReaderIO (Maybe Artist)
selectOneArtistByName name = selectOneMaybeArtistCache (FilterByName name)

-- selectOneArtistByNameAndNameL :: Text -> R.ReaderIO (Maybe Artist)
-- selectOneArtistByNameAndNameL name = selectOneMaybeArtistCache (FilterByArtistNameOrArtistNameL name (toLower name))

selectAllArtist :: R.ReaderIO [Artist]
selectAllArtist = selectAllArtistCache
