module Storage.Queries.DBQueries where

import qualified Database.Beam as B
import qualified Database.Beam.Backend as B
import Database.Beam.Schema.Tables (FieldsFulfillConstraint, HasConstraint)
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import Database.SQLite.Simple (Connection)
import qualified Reader as R
import Control.Monad.Trans.Class (MonadTrans(lift))

selectOneMaybe ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Sqlite
  ) =>
  B.DatabaseEntity be db (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  R.ReaderIO (Maybe (table B.Identity))
selectOneMaybe dbTable predicate = do
  conn <- R.getSqlConnection
  lift $ runSelectOneMaybe conn
  where
    runSelectOneMaybe conn = do
      runBeamSqliteDebug putStrLn conn $
        B.runSelectReturningOne $
          B.select $
            B.filter_' predicate $
              B.all_ dbTable

selectMany ::
  ( B.Beamable table,
    B.Database be db,
    B.FromBackendRow be (table B.Identity),
    be ~ Sqlite
  ) =>
  B.DatabaseEntity be db (B.TableEntity table) ->
  (table (B.QExpr be B.QBaseScope) -> B.QExpr be B.QBaseScope B.SqlBool) ->
  R.ReaderIO [(table B.Identity)]
selectMany dbTable predicate = do
  conn <- R.getSqlConnection
  lift $ runSelectMany conn
  where
    runSelectMany conn = do
      runBeamSqliteDebug putStrLn conn $
        B.runSelectReturningList $
          B.select $
            B.filter_' predicate $
              B.all_ dbTable
