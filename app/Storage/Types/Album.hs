module Storage.Types.Album where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, Table (..))
import Storage.Types.Artist

data AlbumT f = Album
  { albumId :: Columnar f Int32,
    albumTitle :: Columnar f Text,
    albumArtist :: PrimaryKey ArtistT f
  }
  deriving (Generic)

instance Beamable AlbumT

type Album = AlbumT Identity

deriving instance Show Album

instance Table AlbumT where
  data PrimaryKey AlbumT f = AlbumId (Columnar f Int32)
    deriving (Generic)
  primaryKey = AlbumId . albumId

instance Beamable (PrimaryKey AlbumT)

type AlbumId = PrimaryKey AlbumT Identity

deriving instance Show AlbumId

deriving instance Show (PrimaryKey AlbumT (Nullable Identity))
