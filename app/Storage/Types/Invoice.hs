module Storage.Types.Invoice where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Data.Time (LocalTime)
import Database.Beam (Beamable, Columnar, Generic, Identity, Table (..))
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial)
import Storage.Types.Address
import Storage.Types.Customer

data InvoiceT f = Invoice
  { invoiceId :: Columnar f (SqlSerial Int32), -- Slightly different from the standard chinook schema. Used for illustrative purposes in the docs
    invoiceCustomer :: PrimaryKey CustomerT f,
    invoiceDate :: Columnar f LocalTime,
    invoiceBillingAddress :: AddressMixin f,
    invoiceTotal :: Columnar f Scientific
  }
  deriving (Generic)

instance Beamable InvoiceT

type Invoice = InvoiceT Identity

deriving instance Show Invoice

instance Table InvoiceT where
  data PrimaryKey InvoiceT f = InvoiceId (Columnar f (SqlSerial Int32)) deriving (Generic)
  primaryKey = InvoiceId . invoiceId

instance Beamable (PrimaryKey InvoiceT)

type InvoiceId = PrimaryKey InvoiceT Identity

deriving instance Show InvoiceId
