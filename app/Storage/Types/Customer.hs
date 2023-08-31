module Storage.Types.Customer where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, Table (..))
import Storage.Types.Address
import Storage.Types.Employee

data CustomerT f = Customer
  { customerId :: Columnar f Int32,
    customerFirstName :: Columnar f Text,
    customerLastName :: Columnar f Text,
    customerCompany :: Columnar f (Maybe Text),
    customerAddress :: AddressMixin f,
    customerPhone :: Columnar f (Maybe Text),
    customerFax :: Columnar f (Maybe Text),
    customerEmail :: Columnar f Text,
    customerSupportRep :: PrimaryKey EmployeeT (Nullable f)
  }
  deriving (Generic)

instance Beamable CustomerT

type Customer = CustomerT Identity

deriving instance Show Customer

instance Table CustomerT where
  data PrimaryKey CustomerT f = CustomerId (Columnar f Int32)
    deriving (Generic)
  primaryKey = CustomerId . customerId

instance Beamable (PrimaryKey CustomerT)

type CustomerId = PrimaryKey CustomerT Identity

deriving instance Show CustomerId
