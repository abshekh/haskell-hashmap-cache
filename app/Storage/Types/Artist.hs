module Storage.Types.Artist where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Table (..))

data ArtistT f = Artist
  { artistId :: Columnar f Int32,
    artistName :: Columnar f Text
  }
  deriving (Generic)

instance Beamable ArtistT

type Artist = ArtistT Identity

deriving instance Show Artist

instance Table ArtistT where
  data PrimaryKey ArtistT f = ArtistId (Columnar f Int32)
    deriving (Generic)
  primaryKey = ArtistId . artistId

instance Beamable (PrimaryKey ArtistT)

type ArtistId = PrimaryKey ArtistT Identity

deriving instance Show ArtistId
