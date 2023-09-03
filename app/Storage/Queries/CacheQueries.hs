module Storage.Queries.CacheQueries where

import Control.Concurrent (modifyMVar_, readMVar)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.HashMap.Strict as HM
import Data.Text hiding (foldr, map, null)
import Reader as R
import Storage.Types.Cache

selectOneMaybe :: String -> String -> R.ReaderIO (Maybe Cache)
selectOneMaybe prefix key = do
  cache' <- R.getCache
  cache <- lift $ readMVar cache'
  return $ selectOneHelper cache
  where
    selectOneHelper cache = do
      fkey <- HM.lookup (getKey prefix key) cache
      case fkey of
        ForeignKey fkey' -> HM.lookup fkey' cache
        _ -> Nothing

selectMany :: String -> String -> R.ReaderIO [Cache]
selectMany prefix key = do
  cache' <- R.getCache
  cache <- lift $ readMVar cache'
  return $ selectManyHelper cache
  where
    selectManyHelper cache = do
      case HM.lookup (getKey prefix key) cache of
        Just (ForeignKeys fkeys) -> do
          foldr
            ( \curr acc ->
                case HM.lookup curr cache of
                  Just v -> v : acc
                  Nothing -> acc
            )
            []
            fkeys
        _ -> []

updateOne :: (String, String) -> Cache -> R.ReaderIO ()
updateOne (prefix, fkey) value = do
  cache <- R.getCache
  let foreignKey = pack $ prefix ++ "-" ++ fkey ++ "-fkey"
  lift $ modifyMVar_ cache (return . HM.insert foreignKey value)

insertOne' :: String -> String -> String -> Cache -> R.ReaderIO ()
insertOne' prefix key fkey value = do
  cache <- R.getCache
  let foreignKey = getForeignKey prefix fkey
  lift $ modifyMVar_ cache (return . HM.insert foreignKey value)
  lift $ modifyMVar_ cache (return . HM.insert (getKey prefix key) (ForeignKey foreignKey))

insertOne :: String -> [String] -> String -> Cache -> R.ReaderIO ()
insertOne prefix key fkey value = do
  cache <- R.getCache
  let foreignKey = getForeignKey prefix fkey
  lift $ modifyMVar_ cache (return . HM.insert foreignKey value)
  lift $
    forM_ key $
      \k ->
        modifyMVar_
          cache
          ( return
              . HM.insert
                (getKey prefix k)
                (ForeignKey foreignKey)
          )

insertMany' :: String -> String -> String -> [Cache] -> R.ReaderIO ()
insertMany' prefix key fkey = mapM_ (insertOne' prefix key fkey)

insertMany :: String -> [String] -> String -> [Cache] -> R.ReaderIO ()
insertMany prefix keys fkey = mapM_ (insertOne prefix keys fkey)

selectOrInsertInCache ::
  (Foldable m, Traversable m, Show f) =>
  (f -> ReaderIO (m a)) ->
  (f -> ReaderIO (m a)) ->
  (a -> f -> ReaderIO b) ->
  f ->
  ReaderIO (m a)
selectOrInsertInCache selectCache selectDB insertCache filterBy = do
  r <- selectCache filterBy
  if null r
    then do
      r' <- selectDB filterBy
      lift $ putStrLn $ "Not Found in cache, found in DB for filterBy: " ++ show filterBy
      mapM_ (`insertCache` filterBy) r'
      return r'
    else do
      lift $ putStrLn $ "Found in cache for filterBy: " ++ show filterBy
      return r

getKey :: String -> String -> Text
getKey prefix key = pack $ prefix ++ "-" ++ key

getForeignKey :: String -> String -> Text
getForeignKey prefix fkey = pack $ prefix ++ "-" ++ fkey ++ "-fkey"
