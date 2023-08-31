module Storage.Types.Address where

import Data.Text (Text)
import Database.Beam ( Generic, Beamable, Identity, Columnar )

data AddressMixin f = Address
  { address :: Columnar f (Maybe Text),
    addressCity :: Columnar f (Maybe Text),
    addressState :: Columnar f (Maybe Text),
    addressCountry :: Columnar f (Maybe Text),
    addressPostalCode :: Columnar f (Maybe Text)
  }
  deriving (Generic)

instance Beamable AddressMixin

type Address = AddressMixin Identity

deriving instance Show (AddressMixin Identity)
