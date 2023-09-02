module Storage.Queries.CacheQueries where

import Control.Concurrent (modifyMVar_, readMVar)
import Control.Monad (forM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Data.HashMap.Strict as HM
import Data.Text
import Reader as R
import Storage.Types.Cache

selectOneMaybe :: (String, String) -> R.ReaderIO (Maybe Cache)
selectOneMaybe (prefix, key) = do
  cache' <- R.getCache
  cache <- lift $ readMVar cache'
  return $ selectOneHelper cache
  where
    selectOneHelper cache = do
      fkey <- HM.lookup (pack $ prefix ++ key) cache
      case fkey of
        ForeignKey fkey' -> HM.lookup fkey' cache
        _ -> Nothing

updateOne :: (String, String) -> Cache -> R.ReaderIO ()
updateOne (prefix, fkey) value = do
  cache <- R.getCache
  lift $ modifyMVar_ cache (return . HM.insert (pack $ prefix ++ fkey ++ "fkey") value)

insertOne' :: (String, String, String) -> Cache -> R.ReaderIO ()
insertOne' (prefix, key, fkey) value = do
  cache <- R.getCache
  lift $ modifyMVar_ cache (return . HM.insert (pack $ prefix ++ fkey ++ "fkey") value)
  lift $ modifyMVar_ cache (return . HM.insert (pack $ prefix ++ key) (ForeignKey $ pack $ prefix ++ fkey ++ "fkey"))

insertOne :: (String, [String], String) -> Cache -> R.ReaderIO ()
insertOne (prefix, key, fkey) value = do
  cache <- R.getCache
  lift $ modifyMVar_ cache (return . HM.insert (pack $ prefix ++ fkey ++ "fkey") value)
  lift $
    forM_ key $
      \k ->
        modifyMVar_
          cache
          ( return
              . HM.insert
                (pack $ prefix ++ k)
                (ForeignKey $ pack $ prefix ++ fkey ++ "fkey")
          )

selectOrInsertInCache ::
  (t -> ReaderIO (Maybe a1)) ->
  (t -> ReaderIO (Maybe a1)) ->
  (a1 -> t -> ReaderIO ()) ->
  t ->
  ReaderIO (Maybe a1)
selectOrInsertInCache selectOneMaybeCache selectOneMaybeDB insertOneCache filterBy = do
  r <- selectOneMaybeCache filterBy
  case r of
    Just _ -> do
      lift $ putStrLn "Found in cache"
      return r
    Nothing -> do
      r' <- selectOneMaybeDB filterBy
      lift $ putStrLn "Not Found in cache, found in DB"
      case r' of
        Just val -> do
          insertOneCache val filterBy
          return r'
        _ -> return Nothing
