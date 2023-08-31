module Storage.Types.Track where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, Table (..))
import Storage.Types.Album
import Storage.Types.Genre
import Storage.Types.MediaType

data TrackT f = Track
  { trackId :: Columnar f Int32,
    trackName :: Columnar f Text,
    trackAlbumId :: PrimaryKey AlbumT (Nullable f),
    trackMediaTypeId :: PrimaryKey MediaTypeT f,
    trackGenreId :: PrimaryKey GenreT (Nullable f),
    trackComposer :: Columnar f (Maybe Text),
    trackMilliseconds :: Columnar f Int32,
    trackBytes :: Columnar f Int32,
    trackUnitPrice :: Columnar f Scientific
  }
  deriving (Generic)

instance Beamable TrackT

type Track = TrackT Identity

deriving instance Show Track

instance Table TrackT where
  data PrimaryKey TrackT f = TrackId (Columnar f Int32) deriving (Generic)
  primaryKey = TrackId . trackId

instance Beamable (PrimaryKey TrackT)

type TrackId = PrimaryKey TrackT Identity

deriving instance Show TrackId
