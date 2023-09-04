module Storage.Types.Cache where

import Data.Text
import Storage.Types.Album
import Storage.Types.Artist
import Storage.Types.Customer
-- import Storage.Types.Employee
-- import Storage.Types.Genre
-- import Storage.Types.Invoice
-- import Storage.Types.InvoiceLine
-- import Storage.Types.MediaType
-- import Storage.Types.Playlist
-- import Storage.Types.PlaylistTrack
-- import Storage.Types.Track

data Cache
  = AlbumCache Album
  | ArtistCache Artist
  | CustomerCache Customer
  | ForeignKey Text
  | ForeignKeys [Text]
  deriving (Show)
