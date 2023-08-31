module Storage.Types.InvoiceLine where

import Data.Int (Int32)
import Data.Scientific (Scientific)
import Database.Beam (Beamable, Columnar, Generic, Identity, Table (..))
import Storage.Types.Invoice
import Storage.Types.Track

data InvoiceLineT f = InvoiceLine
  { invoiceLineId :: Columnar f Int32,
    invoiceLineInvoice :: PrimaryKey InvoiceT f,
    invoiceLineTrack :: PrimaryKey TrackT f,
    invoiceLineUnitPrice :: Columnar f Scientific,
    invoiceLineQuantity :: Columnar f Int32
  }
  deriving (Generic)

instance Beamable InvoiceLineT

type InvoiceLine = InvoiceLineT Identity

deriving instance Show InvoiceLine

instance Table InvoiceLineT where
  data PrimaryKey InvoiceLineT f = InvoiceLineId (Columnar f Int32) deriving (Generic)
  primaryKey = InvoiceLineId . invoiceLineId

instance Beamable (PrimaryKey InvoiceLineT)

type InvoiceLineId = PrimaryKey InvoiceLineT Identity

deriving instance Show InvoiceLineId
