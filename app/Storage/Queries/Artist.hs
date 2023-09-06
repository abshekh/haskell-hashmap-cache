{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}

module Storage.Queries.Artist where

import Control.Monad.Extra
import Data.Data (Proxy (Proxy))
import Data.Int (Int32)
import Data.Maybe
import Data.Text (Text)
import Database.Beam ((&&.), (==.), (||.))
import qualified Database.Beam as B
import Database.Beam.Sqlite (Sqlite)
import qualified Reader as R
import qualified Storage.Queries.CacheQueries as CQ
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Artist
import Storage.FilterBy.Artist
import Storage.Types.Cache
import Storage.Types.CacheClass
import Storage.Types.CacheChannel
import qualified Storage.Types.DB as DB
import Control.Monad.Trans.Class (MonadTrans(lift))

getDbTable :: B.DatabaseEntity be DB.ChinookDb (B.TableEntity ArtistT)
getDbTable = DB._artist DB.chinookDb

selectOneMaybeArtist :: FilterByOne -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtist filterBy = do
  let predicate = getPredicate filterBy
      dbTable = getDbTable
  Q.selectOneMaybe dbTable predicate

selectOneMaybeArtistCache :: FilterByOne -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtistCache filterBy = do
  let keyName = getKey (Proxy @Artist) filterBy
  artist <- selectOneMaybeArtistCacheHelper keyName
  case artist of
    Just _ -> do
      lift $ putStrLn $ "Found in cache: " ++ show filterBy
      return artist
    Nothing -> do
      lift $ putStrLn $ "Not found in cache querying DB: " ++ show filterBy
      cacheChannel <- R.getCacheChannel
      artist' <- selectOneMaybeArtist filterBy
      let artistCacheChannel = _artistCacheQueue cacheChannel
      whenJust artist' $ \a -> CQ.insertOne keyName a (Proxy @FilterByOne) artistCacheChannel
      return artist'
  where
    selectOneMaybeArtistCacheHelper keyName = do
      cache <- R.getCache
      let artistCache = _artistCache cache
      CQ.selectOneMaybe keyName artistCache

-- selectOneMaybeArtistCache :: FilterByOne -> R.ReaderIO (Maybe Artist)
-- selectOneMaybeArtistCache =
--   CQ.selectOrInsertInCache
--     selectOneMaybeArtistCacheHelper
--     selectOneMaybeArtist
--     insertOneArtistCache
--   where
--     selectOneMaybeArtistCacheHelper filterBy = do
--       let prefix = getDbTableName
--           keyName = getKey (Proxy :: Proxy Artist) filterBy
--       cache <- CQ.selectOneMaybe prefix keyName
--       return $ case cache of
--         Just (ArtistCache artist) -> Just artist
--         _ -> Nothing

-- insertOneArtistCache :: Maybe Artist -> FilterByOne -> R.ReaderIO ()
-- insertOneArtistCache (Just artist) filterBy = do
--   let prefix = getDbTableName
--       fkey = show $ B.primaryKey artist
--       keyName = getKey (Proxy :: Proxy Artist) filterBy
--       allKeys' = getAllKeys artist (Proxy :: Proxy FilterByOne)
--       allKeys = keyName : allKeys'
--   CQ.insertOne prefix allKeys fkey (ArtistCache artist)
-- insertOneArtistCache Nothing _ = return ()

getPredicate ::
  FilterByOne ->
  ArtistT (B.QGenExpr B.QValueContext Sqlite s) ->
  B.QGenExpr B.QValueContext Sqlite s B.SqlBool
getPredicate (FilterById id') = \Artist {..} -> B.sqlBool_ (artistId ==. B.val_ id')
getPredicate (FilterByName name) = \Artist {..} -> B.sqlBool_ (artistName ==. B.val_ name)

-- getPredicate (FilterByArtistIdAndArtistName id' name) = \Artist {..} -> B.sqlBool_ ((artistId ==. B.val_ id') &&. (artistName ==. B.val_ name))
-- getPredicate (FilterByArtistNameOrArtistNameL name nameL) = \Artist {..} -> B.sqlBool_ ((artistName ==. B.val_ name) ||. (artistName ==. B.val_ nameL))
