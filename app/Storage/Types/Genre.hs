module Storage.Types.Genre where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, Table (..))

data GenreT f = Genre
  { genreId :: Columnar f Int32,
    genreName :: Columnar f Text
  }
  deriving (Generic)

instance Beamable GenreT

type Genre = GenreT Identity

deriving instance Show Genre

instance Table GenreT where
  data PrimaryKey GenreT f = GenreId (Columnar f Int32)
    deriving (Generic)
  primaryKey = GenreId . genreId

instance Beamable (PrimaryKey GenreT)

type GenreId = PrimaryKey GenreT Identity

deriving instance Show GenreId

deriving instance Show (PrimaryKey GenreT (Nullable Identity))
