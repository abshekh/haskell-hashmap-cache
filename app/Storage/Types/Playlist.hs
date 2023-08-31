module Storage.Types.Playlist where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Table (..))

data PlaylistT f = Playlist
  { playlistId :: Columnar f Int32,
    playlistName :: Columnar f (Maybe Text)
  }
  deriving (Generic)

instance Beamable PlaylistT

type Playlist = PlaylistT Identity

deriving instance Show Playlist

instance Table PlaylistT where
  data PrimaryKey PlaylistT f = PlaylistId (Columnar f Int32) deriving (Generic)
  primaryKey = PlaylistId . playlistId

instance Beamable (PrimaryKey PlaylistT)

type PlaylistId = PrimaryKey PlaylistT Identity

deriving instance Show PlaylistId
