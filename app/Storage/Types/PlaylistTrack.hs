module Storage.Types.PlaylistTrack where

import Database.Beam (Beamable, Generic, Identity, Table (..))
import Storage.Types.Playlist
import Storage.Types.Track

data PlaylistTrackT f = PlaylistTrack
  { playlistTrackPlaylistId :: PrimaryKey PlaylistT f,
    playlistTrackTrackId :: PrimaryKey TrackT f
  }
  deriving (Generic)

instance Beamable PlaylistTrackT

type PlaylistTrack = PlaylistTrackT Identity

deriving instance Show PlaylistTrack

instance Table PlaylistTrackT where
  data PrimaryKey PlaylistTrackT f = PlaylistTrackId (PrimaryKey PlaylistT f) (PrimaryKey TrackT f)
    deriving (Generic)
  primaryKey = PlaylistTrackId <$> playlistTrackPlaylistId <*> playlistTrackTrackId

instance Beamable (PrimaryKey PlaylistTrackT)

type PlaylistTrackId = PrimaryKey PlaylistTrackT Identity

deriving instance Show PlaylistTrackId
