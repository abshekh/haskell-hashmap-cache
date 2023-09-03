module Storage.Queries.Artist where

import Data.Data (Proxy (Proxy))
import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam ((==.), (||.))
import qualified Database.Beam as B
import qualified Reader as R
import qualified Storage.Queries.CacheQueries as CQ
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Artist
import Storage.Types.Cache (Cache (ArtistCache))
import Storage.Types.CacheClass
import Storage.Types.CacheTH
import qualified Storage.Types.DB as DB
import Database.Beam.Sqlite (Sqlite)

getDbTable :: B.DatabaseEntity be DB.ChinookDb (B.TableEntity ArtistT)
getDbTable = DB._artist DB.chinookDb

getDbTableName :: String
getDbTableName = "Artist"

data FilterByOne
  = FilterByName {artistName :: Text}
  | FilterById {artistId :: Int32}
  | FilterByArtistNameOrArtistNameL {artistName :: Text, artistNameL :: Text}
  deriving (Show)

$(deriveCacheClass ''Artist ''FilterByOne)

selectOneMaybeArtist :: FilterByOne -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtist filterBy = do
  let predicate = getPredicate filterBy
      dbTable = getDbTable
  Q.selectOneMaybe dbTable predicate

selectOneMaybeArtistCache :: FilterByOne -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtistCache =
  CQ.selectOrInsertInCache
    selectOneMaybeArtistCacheHelper
    selectOneMaybeArtist
    insertOneArtistCache
  where
    selectOneMaybeArtistCacheHelper filterBy = do
      let prefix = getDbTableName
          keyName = getKey (Proxy :: Proxy Artist) filterBy
      cache <- CQ.selectOneMaybe prefix keyName
      return $ case cache of
        Just (ArtistCache artist) -> Just artist
        _ -> Nothing

insertOneArtistCache :: Artist -> FilterByOne -> R.ReaderIO ()
insertOneArtistCache artist filterBy = do
  let prefix = getDbTableName
      fkey = show $ B.primaryKey artist
      keyName = getKey (Proxy :: Proxy Artist) filterBy
      allKeys' = getAllKeys artist (Proxy :: Proxy FilterByOne)
      allKeys = keyName : allKeys'
  CQ.insertOne prefix allKeys fkey (ArtistCache artist)


getPredicate ::
     FilterByOne
  -> ArtistT (B.QGenExpr B.QValueContext Sqlite s)
  -> B.QGenExpr B.QValueContext Sqlite s B.SqlBool
getPredicate (FilterById id') = \Artist {..} -> B.sqlBool_ (artistId ==. B.val_ id')
getPredicate (FilterByName name) = \Artist {..} -> B.sqlBool_ (artistName ==. B.val_ name)
getPredicate (FilterByArtistNameOrArtistNameL name nameL) = \Artist {..} -> B.sqlBool_ ((artistName ==. B.val_ name) ||. (artistName ==. B.val_ nameL))

-- getKeyName :: FilterBy -> String
-- getKeyName (FilterById id') = "FilterById" ++ show id'
-- getKeyName (FilterByName name) = "FilterByName" ++ show name

-- getAllKeyNames :: Artist -> [String]
-- getAllKeyNames artist =
--   map
--     getKeyName
--     [ FilterById $ artistId artist,
--       FilterByName $ artistName artist
--     ]
