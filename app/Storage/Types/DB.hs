module Storage.Types.DB where

import Data.String (IsString (fromString))
import Database.Beam
  ( Database,
    DatabaseSettings,
    Generic,
    TableEntity,
    TableLens (TableLens),
    dbLenses,
    dbModification,
    defaultDbSettings,
    int,
    modifyTableFields,
    nationalVarchar,
    setEntityName,
    withDbModification,
  )
import Database.Beam.Migrate
  ( CheckedDatabaseSettings,
    Migration,
    MigrationSteps,
    createTable,
    evaluateDatabase,
    field,
    migrationStep,
    notNull,
    unCheckDatabase,
    unique,
  )
import Database.Beam.Migrate.SQL.Tables (field)
import Database.Beam.Migrate.Simple
  ( BringUpToDateHooks (runIrreversibleHook),
    bringUpToDateWithHooks,
    defaultUpToDateHooks,
  )
import Database.Beam.Sqlite (Sqlite, runBeamSqliteDebug)
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Database.SQLite.Simple (Connection)
import qualified Storage.Types.Address as Address
import qualified Storage.Types.Album as Album
import qualified Storage.Types.Artist as Artist
import qualified Storage.Types.Customer as Customer
import qualified Storage.Types.Employee as Employee
import qualified Storage.Types.Genre as Genre
import qualified Storage.Types.Invoice as Invoice
import qualified Storage.Types.InvoiceLine as InvoiceLine
import qualified Storage.Types.MediaType as MediaType
import qualified Storage.Types.Playlist as Playlist
import qualified Storage.Types.PlaylistTrack as PlaylistTrack
import qualified Storage.Types.Track as Track

data ChinookDb entity = ChinookDb
  { _album :: entity (TableEntity Album.AlbumT),
    _artist :: entity (TableEntity Artist.ArtistT),
    _customer :: entity (TableEntity Customer.CustomerT),
    _employee :: entity (TableEntity Employee.EmployeeT),
    _genre :: entity (TableEntity Genre.GenreT),
    _invoice :: entity (TableEntity Invoice.InvoiceT),
    _invoiceLine :: entity (TableEntity InvoiceLine.InvoiceLineT),
    _mediaType :: entity (TableEntity MediaType.MediaTypeT),
    _playlist :: entity (TableEntity Playlist.PlaylistT),
    _playlistTrack :: entity (TableEntity PlaylistTrack.PlaylistTrackT),
    _track :: entity (TableEntity Track.TrackT)
  }
  deriving (Generic)

instance Database be ChinookDb

addressFields b =
  Address.Address
    (fromString (b <> "Address"))
    (fromString (b <> "City"))
    (fromString (b <> "State"))
    (fromString (b <> "Country"))
    (fromString (b <> "PostalCode"))

chinookDb :: DatabaseSettings be ChinookDb
chinookDb =
  defaultDbSettings
    `withDbModification` ( dbModification
                             { _album =
                                 setEntityName "Album"
                                   <> modifyTableFields (Album.Album "AlbumId" "Title" (Artist.ArtistId "ArtistId")),
                               _artist =
                                 setEntityName "Artist"
                                   <> modifyTableFields (Artist.Artist "ArtistId" "Name"),
                               _customer =
                                 setEntityName "Customer"
                                   <> modifyTableFields
                                     ( Customer.Customer
                                         "CustomerId"
                                         "FirstName"
                                         "LastName"
                                         "Company"
                                         (addressFields "")
                                         "Phone"
                                         "Fax"
                                         "Email"
                                         (Employee.EmployeeId "SupportRepId")
                                     ),
                               _employee =
                                 setEntityName "Employee"
                                   <> modifyTableFields
                                     ( Employee.Employee
                                         "EmployeeId"
                                         "LastName"
                                         "FirstName"
                                         "Title"
                                         (Employee.EmployeeId "ReportsTo")
                                         "BirthDate"
                                         "HireDate"
                                         (addressFields "")
                                         "Phone"
                                         "Fax"
                                         "Email"
                                     ),
                               _genre =
                                 setEntityName "Genre"
                                   <> modifyTableFields
                                     (Genre.Genre "GenreId" "Name"),
                               _invoice =
                                 setEntityName "Invoice"
                                   <> modifyTableFields
                                     ( Invoice.Invoice
                                         "InvoiceId"
                                         (Customer.CustomerId "CustomerId")
                                         "InvoiceDate"
                                         (addressFields "Billing")
                                         "Total"
                                     ),
                               _invoiceLine =
                                 setEntityName "InvoiceLine"
                                   <> modifyTableFields
                                     ( InvoiceLine.InvoiceLine
                                         "InvoiceLineId"
                                         (Invoice.InvoiceId "InvoiceId")
                                         (Track.TrackId "TrackId")
                                         "UnitPrice"
                                         "Quantity"
                                     ),
                               _mediaType =
                                 setEntityName "MediaType"
                                   <> modifyTableFields (MediaType.MediaType "MediaTypeId" "Name"),
                               _playlist =
                                 setEntityName "Playlist"
                                   <> modifyTableFields (Playlist.Playlist "PlaylistId" "Name"),
                               _playlistTrack =
                                 setEntityName "PlaylistTrack"
                                   <> modifyTableFields
                                     ( PlaylistTrack.PlaylistTrack
                                         (Playlist.PlaylistId "PlaylistId")
                                         (Track.TrackId "TrackId")
                                     ),
                               _track =
                                 setEntityName "Track"
                                   <> modifyTableFields
                                     ( Track.Track
                                         "TrackId"
                                         "Name"
                                         (Album.AlbumId "AlbumId")
                                         (MediaType.MediaTypeId "MediaTypeId")
                                         (Genre.GenreId "GenreId")
                                         "Composer"
                                         "Milliseconds"
                                         "Bytes"
                                         "UnitPrice"
                                     )
                             }
                         )

-- ChinookDb
--   (TableLens album)
--   (TableLens artist)
--   (TableLens customer)
--   (TableLens employee)
--   (TableLens genre)
--   (TableLens invoice)
--   (TableLens invoiceLine)
--   (TableLens mediaType)
--   (TableLens playlist)
--   (TableLens playlistTrack)
--   (TableLens track) = dbLenses

