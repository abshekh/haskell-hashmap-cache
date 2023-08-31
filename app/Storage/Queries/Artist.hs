module Storage.Queries.Artist where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam ((==.))
import qualified Database.Beam as B
import qualified Reader as R
import qualified Storage.Queries.CacheQueries as CQ
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Artist
import Storage.Types.Cache (Cache (ArtistCache))
import qualified Storage.Types.DB as DB

getDbTable :: B.DatabaseEntity be DB.ChinookDb (B.TableEntity ArtistT)
getDbTable = DB._artist DB.chinookDb

getDbTableName :: String
getDbTableName = "Artist"

data FilterBy
  = FilterByName Text
  | FilterById Int32

selectOneMaybeArtist :: FilterBy -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtist filterBy = do
  let predicate = getPredicate filterBy
      dbTable = getDbTable
  Q.selectOneMaybe dbTable predicate

selectOneMaybeArtistCache :: FilterBy -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtistCache =
  CQ.selectOrInsertInCache
    selectOneMaybeArtistCacheHelper
    selectOneMaybeArtist
    insertOneArtistCache
  where
    selectOneMaybeArtistCacheHelper filterBy = do
      let prefix = getDbTableName
          key = getKeyName filterBy
      cache <- CQ.selectOneMaybe (prefix, key)
      return $ case cache of
        Just (ArtistCache artist) -> Just artist
        _ -> Nothing

insertOneArtistCache :: Artist -> R.ReaderIO ()
insertOneArtistCache artist = do
  let prefix = getDbTableName
      fkey = show $ artistId artist
      allKeys = getAllKeyNames artist
  CQ.insertOne (prefix, allKeys, fkey) (ArtistCache artist)

getPredicate ::
  ( B.HaskellLiteralForQExpr (B.Columnar f Int32) ~ Int32,
    B.HaskellLiteralForQExpr (B.Columnar f Text) ~ Text,
    B.SqlEq (B.QGenExpr context syntax s) (B.Columnar f Int32),
    B.SqlEq (B.QGenExpr context syntax s) (B.Columnar f Text),
    B.SqlValable (B.Columnar f Int32),
    B.SqlValable (B.Columnar f Text)
  ) =>
  FilterBy ->
  ArtistT f ->
  B.QGenExpr context syntax s B.SqlBool
getPredicate (FilterById id') = \Artist {..} -> B.sqlBool_ (artistId ==. B.val_ id')
getPredicate (FilterByName name) = \Artist {..} -> B.sqlBool_ (artistName ==. B.val_ name)

getKeyName :: FilterBy -> String
getKeyName (FilterById id') = "FilterById" ++ show id'
getKeyName (FilterByName name) = "FilterByName" ++ show name

getAllKeyNames :: Artist -> [String]
getAllKeyNames artist =
  map
    getKeyName
    [ FilterById $ artistId artist,
      FilterByName $ artistName artist
    ]
