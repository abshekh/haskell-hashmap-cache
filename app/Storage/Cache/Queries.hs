module Storage.Cache.Queries where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import Control.Monad (void)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Data (Proxy)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, readIORef)
import qualified Data.Text as DT
import Reader as R
import qualified Data.Time as DT

import Storage.Cache.Cache
import Storage.Cache.CacheChannel (CacheQueue)

data CacheError
  = NotFound
  | KeyExpired

selectOneMaybe :: String -> IORef (HM.HashMap DT.Text (CacheValue a)) -> ReaderIO (Either CacheError (Maybe a))
selectOneMaybe keyName cacheIORef = do
  cache <- lift $ readIORef cacheIORef
  selectOneMaybeHelper cache
  where
    selectOneMaybeHelper cache = do
      currentTime <- lift $ getCurrentLocalTime
      case (HM.lookup (DT.pack keyName)) cache of
        Just (ReferenceId (Just s, expiryTime)) -> do
          if currentTime >= expiryTime
            then return (Left KeyExpired)
            else do
              case HM.lookup s cache of
                Just (Value a) -> return (Right $ Just a)
                _ -> return (Right Nothing)
        Just (ReferenceId (Nothing, expiryTime)) -> do
          if currentTime >= expiryTime
            then return (Left KeyExpired)
            else return (Right Nothing)
        _ -> return (Left NotFound)

selectMany :: String -> IORef (HM.HashMap DT.Text (CacheValue a)) -> R.ReaderIO (Either CacheError [a])
selectMany keyName cacheIORef = do
  cache <- lift $ readIORef cacheIORef
  selectManyHelper cache
  where
    selectManyHelper cache = do
      currentTime <- lift $ getCurrentLocalTime
      case HM.lookup (DT.pack keyName) cache of
        Just (ReferenceIds (referenceIds, expiryTime)) -> do
          if currentTime >= expiryTime
            then return (Left KeyExpired)
            else return $
                 Right $
                 foldr
                   (\curr acc ->
                      case HM.lookup curr cache of
                        Just (Value v) -> v : acc
                        _ -> acc)
                   []
                   referenceIds
        _ -> return $ Left NotFound

insert :: String -> [a] -> Proxy f -> Int -> CacheQueue a f -> R.ReaderIO ()
insert key val f expirySecs (inChan, _) = do
  insertionTime <- lift $ getCurrentLocalTimePlusSecs expirySecs
  void $ lift $ Chan.tryWriteChan inChan (key, val, insertionTime, f)


ist :: DT.TimeZone
ist = DT.TimeZone 330 False "IST"

getCurrentLocalTime :: IO DT.LocalTime
getCurrentLocalTime = DT.utcToLocalTime ist <$> DT.getCurrentTime

getCurrentLocalTimePlusSecs :: Int -> IO DT.LocalTime
getCurrentLocalTimePlusSecs secs =
  fmap (DT.addLocalTime (realToFrac secs)) getCurrentLocalTime
