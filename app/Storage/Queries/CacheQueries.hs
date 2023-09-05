module Storage.Queries.CacheQueries where

import Control.Concurrent (modifyMVar_, readMVar)
import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Control.Monad (forM_, void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef)
import Data.Text hiding (foldr, map, null)
import Reader as R
import Storage.Types.Cache

selectOneMaybe :: String -> IORef (HM.HashMap Text (CacheValue a)) -> ReaderIO (Maybe a)
selectOneMaybe keyName cacheIORef = do
  cache <- lift $ readIORef cacheIORef
  return $ selectOneMaybeHelper cache
  where
    selectOneMaybeHelper cache = do
      primaryIdx <- HM.lookup (pack keyName) cache
      case primaryIdx of
        PrimaryIdx s -> do
          secIdx <- HM.lookup s cache
          case secIdx of
            ForeignIdx (_, a) -> return a
            _ -> Nothing
        _ -> Nothing

insertOne :: String -> [String] -> String -> a -> CacheQueue a -> R.ReaderIO ()
insertOne primaryKey secondaryKeys foreignKey value (inChan, _) = do
  let pKey = pack primaryKey
      sKeys = pack <$> secondaryKeys
      fKey = pack foreignKey
      cacheQueueValue =
        CacheQueueValue
          { _primaryKey = pKey,
            _secondaryKeys = sKeys,
            _foriegnKey = fKey,
            _cacheValue = ForeignIdx (pKey : sKeys, value)
          }
  void $ lift $ Chan.tryWriteChan inChan cacheQueueValue

-- selectOneMaybe :: String -> String -> R.ReaderIO (Maybe Cache)
-- selectOneMaybe prefix key = do
--   cache' <- R.getCache
--   cache <- lift $ readMVar cache'
--   return $ selectOneHelper cache
--   where
--     selectOneHelper cache = do
--       fkey <- HM.lookup (getKey prefix key) cache
--       case fkey of
--         ForeignKey fkey' -> HM.lookup fkey' cache
--         _ -> Nothing
--
-- selectMany :: String -> String -> R.ReaderIO [Cache]
-- selectMany prefix key = do
--   cache' <- R.getCache
--   cache <- lift $ readMVar cache'
--   return $ selectManyHelper cache
--   where
--     selectManyHelper cache = do
--       case HM.lookup (getKey prefix key) cache of
--         Just (ForeignKeys fkeys) -> do
--           foldr
--             ( \curr acc ->
--                 case HM.lookup curr cache of
--                   Just v -> v : acc
--                   Nothing -> acc
--             )
--             []
--             fkeys
--         _ -> []
--
-- updateOne :: (String, String) -> Cache -> R.ReaderIO ()
-- updateOne (prefix, fkey) value = do
--   cache <- R.getCache
--   let foreignKey = pack $ prefix ++ "-" ++ fkey ++ "-fkey"
--   lift $ modifyMVar_ cache (return . HM.insert foreignKey value)
--
-- insertOne' :: String -> String -> String -> Cache -> R.ReaderIO ()
-- insertOne' prefix key fkey value = do
--   cache <- R.getCache
--   let foreignKey = getForeignKey prefix fkey
--   lift $ modifyMVar_ cache (return . HM.insert foreignKey value)
--   lift $ modifyMVar_ cache (return . HM.insert (getKey prefix key) (ForeignKey foreignKey))
--
-- insertOne :: String -> [String] -> String -> Cache -> R.ReaderIO ()
-- insertOne prefix key fkey value = do
--   cache <- R.getCache
--   let foreignKey = getForeignKey prefix fkey
--   lift $ modifyMVar_ cache (return . HM.insert foreignKey value)
--   lift $
--     forM_ key $
--       \k ->
--         modifyMVar_
--           cache
--           ( return
--               . HM.insert
--                 (getKey prefix k)
--                 (ForeignKey foreignKey)
--           )
--
-- insertMany :: String -> String -> [([String], String, Cache)] -> R.ReaderIO ()
-- insertMany prefix key args = do
--   cache <- R.getCache
--   fkeys <- mapM insertAndReturnForeignKeys args
--   lift $ modifyMVar_ cache (return . HM.insert (getKey prefix key) (ForeignKeys fkeys))
--   where
--     insertAndReturnForeignKeys (allKeys, fkey, value) = do
--       cache <- R.getCache
--       let foreignKey = getForeignKey prefix fkey
--       lift $ modifyMVar_ cache (return . HM.insert foreignKey value)
--       lift $
--         forM_ allKeys $
--           \k ->
--             modifyMVar_
--               cache
--               ( return
--                   . HM.insert
--                     (getKey prefix k)
--                     (ForeignKey foreignKey)
--               )
--       return foreignKey
--
-- selectOrInsertInCache ::
--   (Foldable m, Traversable m, Show f) =>
--   (f -> ReaderIO (m a)) ->
--   (f -> ReaderIO (m a)) ->
--   (m a -> f -> ReaderIO b) ->
--   f ->
--   ReaderIO (m a)
-- selectOrInsertInCache selectCache selectDB insertCache filterBy = do
--   r <- selectCache filterBy
--   if null r
--     then do
--       r' <- selectDB filterBy
--       lift $ putStrLn $ "Not Found in cache, found in DB for filterBy: " ++ show filterBy
--       void $ insertCache r' filterBy
--       return r'
--     else do
--       lift $ putStrLn $ "Found in cache for filterBy: " ++ show filterBy
--       return r
--
-- getKey :: String -> String -> Text
-- getKey prefix key = pack $ prefix ++ "-" ++ key
--
-- getForeignKey :: String -> String -> Text
-- getForeignKey prefix fkey = pack $ prefix ++ "-" ++ fkey ++ "-fkey"
