module Storage.Queries.Album where

import Data.Data (Proxy (Proxy))
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam ((==.))
import qualified Database.Beam as B
import Database.Beam.Sqlite (Sqlite)
import qualified Reader as R
import qualified Storage.Queries.CacheQueries as CQ
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Album
import Storage.Types.Cache (Cache (AlbumCache))
import Storage.Types.CacheClass
import Storage.Types.CacheTH
import qualified Storage.Types.DB as DB

getDbTable :: B.DatabaseEntity be DB.ChinookDb (B.TableEntity AlbumT)
getDbTable = DB._album DB.chinookDb

getDbTableName :: String
getDbTableName = "Album"

data FilterBy
  = FilterByOne FilterByOneData
  | FilterByMany FilterByManyData

data FilterByOneData
  = FilterByAlbumId {albumId :: Int32}
  | FilterByAlbumTitle {albumTitle :: Text}
  deriving (Show)

newtype FilterByManyData = FilterByArtistId {artistId :: Int32}
  deriving (Show)

$(deriveCacheClass ''Album ''FilterByOneData)

$(deriveCacheClass ''Album ''FilterByManyData)

selectOneMaybeAlbum :: FilterByOneData -> R.ReaderIO (Maybe Album)
selectOneMaybeAlbum filterBy = do
  let predicate = getPredicate (FilterByOne filterBy)
      dbTable = getDbTable
  Q.selectOneMaybe dbTable predicate

selectManyAlbum :: FilterByManyData -> R.ReaderIO [Album]
selectManyAlbum filterBy = do
  let predicate = getPredicate (FilterByMany filterBy)
      dbTable = getDbTable
  Q.selectMany dbTable predicate

selectOneMaybeAlbumCache :: FilterByOneData -> R.ReaderIO (Maybe Album)
selectOneMaybeAlbumCache =
  CQ.selectOrInsertInCache
    selectOneMaybeAlbumCacheHelper
    selectOneMaybeAlbum
    insertOneAlbumCache
  where
    selectOneMaybeAlbumCacheHelper filterBy = do
      let prefix = getDbTableName
          keyName = getKey (Proxy :: Proxy Album) filterBy
      cache <- CQ.selectOneMaybe prefix keyName
      return $ case cache of
        Just (AlbumCache album) -> Just album
        _ -> Nothing

insertOneAlbumCache :: Album -> FilterByOneData -> R.ReaderIO ()
insertOneAlbumCache album filterBy = do
  let prefix = getDbTableName
      fkey = show $ B.primaryKey album
      keyName = getKey (Proxy :: Proxy Album) filterBy
      allKeys' = getAllKeys album (Proxy :: Proxy FilterByOneData)
      allKeys = keyName : allKeys'
  CQ.insertOne prefix allKeys fkey (AlbumCache album)

insertManyAlbumCache :: Album -> FilterByManyData -> R.ReaderIO ()
insertManyAlbumCache album filterBy = do
  let prefix = getDbTableName
      fkey = show $ B.primaryKey album
      keyName = getKey (Proxy :: Proxy Album) filterBy
      allKeys' = getAllKeys album (Proxy :: Proxy FilterByOneData)
      allKeys = keyName : allKeys'
  CQ.insertOne prefix allKeys fkey (AlbumCache album)

selectManyAlbumCache :: FilterByManyData -> R.ReaderIO [Album]
selectManyAlbumCache =
  CQ.selectOrInsertInCache
    selectManyAlbumCacheHelper
    selectManyAlbum
    insertManyAlbumCache
  where
    selectManyAlbumCacheHelper filterBy = do
      let prefix = getDbTableName
          keyName = getKey (Proxy :: Proxy Album) filterBy
      cache <- CQ.selectMany prefix keyName
      return $
        foldr
          ( \c acc ->
              case c of
                (AlbumCache a) -> a : acc
                _ -> acc
          )
          []
          cache

getPredicate ::
  FilterBy ->
  AlbumT (B.QGenExpr B.QValueContext Sqlite s) ->
  B.QGenExpr B.QValueContext Sqlite s B.SqlBool
getPredicate (FilterByOne (FilterByAlbumId id')) = \Album {..} -> B.sqlBool_ (albumId ==. B.val_ id')
getPredicate (FilterByOne (FilterByAlbumTitle title)) = \Album {..} -> B.sqlBool_ (albumTitle ==. B.val_ title)
getPredicate (FilterByMany (FilterByArtistId id')) = \Album {..} -> B.sqlBool_ (artistId ==. B.val_ id')
