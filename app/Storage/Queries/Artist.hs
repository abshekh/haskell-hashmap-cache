{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Storage.Queries.Artist where

import Control.Lens
import Control.Monad.Extra
import Data.Data (Proxy (Proxy))
import Data.Int
import Data.Maybe
import Data.Text
import Database.Beam ((==.))
import qualified Database.Beam as B
import Database.Beam.Sqlite (Sqlite)
import qualified Reader as R
import qualified Storage.Queries.DBQueries as Q
import Storage.Types.Artist
import Storage.FilterBy.Artist
import qualified Storage.Cache.Queries as CQ
import Storage.Cache.Cache as C
import Storage.Cache.CacheChannel as CH
import qualified Storage.Types.DB as DB
import Control.Monad.Trans.Class (MonadTrans(lift))

getDbTable :: B.DatabaseEntity be DB.ChinookDb (B.TableEntity ArtistT)
getDbTable = DB._artist DB.chinookDb

selectOneMaybeArtist :: FilterByOne -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtist filterBy = do
  let predicate = getPredicate filterBy
      dbTable = getDbTable
  Q.selectOneMaybe dbTable predicate

selectAllArtist :: R.ReaderIO [Artist]
selectAllArtist = Q.selectAll getDbTable

selectOneMaybeArtistCache :: FilterByOne -> R.ReaderIO (Maybe Artist)
selectOneMaybeArtistCache filterBy = do
  let keyName = getKey (Proxy @Artist) filterBy
  artist <- selectOneMaybeArtistCacheHelper keyName
  case artist of
    Right a -> do
      lift $ putStrLn $ "Found in cache: " ++ show filterBy
      return a
    Left _ -> do
      lift $ putStrLn $ "Not found in cache querying DB: " ++ show filterBy
      cacheChannel <- R.getCacheChannel
      artist' <- selectOneMaybeArtist filterBy
      let artistCacheChannel = cacheChannel ^. CH.artistCache
      let artist'' = case artist' of
                              Nothing -> []
                              Just a -> [a]
      CQ.insert keyName artist'' (Proxy @FilterByOne) 5 artistCacheChannel
      return artist'
  where
    selectOneMaybeArtistCacheHelper keyName = do
      cache <- R.getCache
      let artistCache' = cache ^. C.artistCache
      CQ.selectOneMaybe keyName artistCache'

selectAllArtistCache :: R.ReaderIO [Artist]
selectAllArtistCache = do
  let keyName = "Table"
  artist <- selectAllArtistCacheHelper keyName
  case artist of
    Left _ -> do
      lift $ putStrLn "Not found all artists in cache querying DB"
      cacheChannel <- R.getCacheChannel
      artist' <- selectAllArtist
      let artistCacheChannel = cacheChannel ^. CH.artistCache
      CQ.insert keyName artist' (Proxy @FilterByOne) 5 artistCacheChannel
      return artist'
    Right a -> do
      lift $ putStrLn "Found all artists in cache"
      return a
  where
    selectAllArtistCacheHelper keyName = do
      cache <- R.getCache
      let artistCache' = cache ^. C.artistCache
      CQ.selectMany keyName artistCache'

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
