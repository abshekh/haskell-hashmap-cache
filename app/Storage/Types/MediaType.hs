module Storage.Types.MediaType where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Table (..))

data MediaTypeT f = MediaType
  { mediaTypeId :: Columnar f Int32,
    mediaTypeName :: Columnar f (Maybe Text)
  }
  deriving (Generic)

instance Beamable MediaTypeT

type MediaType = MediaTypeT Identity

deriving instance Show MediaType

instance Table MediaTypeT where
  data PrimaryKey MediaTypeT f = MediaTypeId (Columnar f Int32) deriving (Generic)
  primaryKey = MediaTypeId . mediaTypeId

instance Beamable (PrimaryKey MediaTypeT)

type MediaTypeId = PrimaryKey MediaTypeT Identity

deriving instance Show MediaTypeId
