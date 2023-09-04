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
import Control.Monad.Trans.Class (MonadTrans(lift))

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

selectAllAlbum :: R.ReaderIO [Album]
selectAllAlbum = Q.selectAll getDbTable

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

insertOneAlbumCache :: Maybe Album -> FilterByOneData -> R.ReaderIO ()
insertOneAlbumCache (Just album) filterBy = do
  let prefix = getDbTableName
      fkey = show $ B.primaryKey album
      keyName = getKey (Proxy :: Proxy Album) filterBy
      allKeys' = getAllKeys album (Proxy :: Proxy FilterByOneData)
      allKeys = keyName : allKeys'
  CQ.insertOne prefix allKeys fkey (AlbumCache album)
insertOneAlbumCache Nothing _ = return ()

insertManyAlbumCache :: [Album] -> FilterByManyData -> R.ReaderIO ()
insertManyAlbumCache albums filterBy = do
  let keyName = getKey (Proxy :: Proxy Album) filterBy
  let cacheArgs = map getCacheArgs albums
  CQ.insertMany getDbTableName keyName cacheArgs
  where
    getCacheArgs album = do
      let allKeys = getAllKeys album (Proxy :: Proxy FilterByOneData)
          fkey = show $ B.primaryKey album
      (allKeys, fkey, AlbumCache album)

insertManyAlbumCache' :: [Album] -> String -> R.ReaderIO ()
insertManyAlbumCache' albums keyName = do
  let cacheArgs = map getCacheArgs albums
  CQ.insertMany getDbTableName keyName cacheArgs
  where
    getCacheArgs album = do
      let allKeys = getAllKeys album (Proxy :: Proxy FilterByOneData)
          fkey = show $ B.primaryKey album
      (allKeys, fkey, AlbumCache album)


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

selectAllAlbumCache :: R.ReaderIO [Album]
selectAllAlbumCache = do
  albums <- selectAllAlbumCacheHelper
  if null albums then do
    lift $ putStrLn "Not found: all Albums in cache, querying DB"
    albums' <- selectAllAlbum
    if null albums' then return albums'
    else do
      insertManyAlbumCache' albums' "Table"
      return albums'
  else do
    lift $ putStrLn "Found: all Albums in cache"
    return albums
  where
    selectAllAlbumCacheHelper = do
      let prefix = getDbTableName
          keyName = "Table"
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
