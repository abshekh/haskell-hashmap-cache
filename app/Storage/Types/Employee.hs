module Storage.Types.Employee where

import Data.Int (Int32)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam (Beamable, Columnar, Generic, Identity, Nullable, Table (..))
import Storage.Types.Address

data EmployeeT f = Employee
  { employeeId :: Columnar f Int32,
    employeeLastName :: Columnar f Text,
    employeeFirstName :: Columnar f Text,
    employeeTitle :: Columnar f (Maybe Text),
    employeeReportsTo :: PrimaryKey EmployeeT (Nullable f),
    employeeBirthDate :: Columnar f (Maybe LocalTime),
    employeeHireDate :: Columnar f (Maybe LocalTime),
    employeeAddress :: AddressMixin f,
    employeePhone :: Columnar f (Maybe Text),
    employeeFax :: Columnar f (Maybe Text),
    employeeEmail :: Columnar f (Maybe Text)
  }
  deriving (Generic)

instance Beamable EmployeeT

type Employee = EmployeeT Identity

deriving instance Show Employee

instance Table EmployeeT where
  data PrimaryKey EmployeeT f = EmployeeId (Columnar f Int32)
    deriving (Generic)
  primaryKey = EmployeeId . employeeId

instance Beamable (PrimaryKey EmployeeT)

type EmployeeId = PrimaryKey EmployeeT Identity

deriving instance Show EmployeeId

deriving instance Show (PrimaryKey EmployeeT (Nullable Identity))
