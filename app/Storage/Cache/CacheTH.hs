{-# LANGUAGE TemplateHaskell #-}

module Storage.Cache.CacheTH where

import Control.Arrow (Arrow (second))
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Data (Proxy (Proxy))
import Data.Function (on)
import Data.IORef (newIORef)
import Data.List (intersectBy)
import Language.Haskell.TH
import Storage.Cache.Cache

normalizeConstructor :: Con -> Q (Name, [(Name, Type)])
normalizeConstructor con = do
  case con of
    RecC conName fieldTypes -> return (conName, map (\(n, _, t) -> (mkName $ nameBase n, t)) fieldTypes)
    _otherCons -> fail $ "Invalid constructor found: " <> show _otherCons

getTypeSynInfo :: Dec -> Q [(Name, [(Name, Type)])]
getTypeSynInfo (TySynD _ _ (AppT (ConT typeF) _)) = getTypeInfo typeF
getTypeSynInfo dec = fail $ "Failed to get Type Syn Info for " <> show dec

getTypeInfo :: Name -> Q [(Name, [(Name, Type)])]
getTypeInfo dType = do
  info <- reify dType
  case info of
    TyConI dec ->
      case dec of
        DataD _ _ _ _ consL _ -> mapM normalizeConstructor consL
        NewtypeD _ _ _ _ con _ -> sequence [normalizeConstructor con]
        type'@TySynD {} -> getTypeSynInfo type'
        _otherDec -> fail $ nameBase dType <> " is not a DateD, NewTypeD or TypeDataD"
    _otherInfo -> fail $ nameBase dType <> " is not a type definition"

handleGetKey :: Name -> Q Exp
handleGetKey filterByName = do
  let filterBy = mkName "_filterBy"
      showE = mkName "show"
  filterTypeInfo <- getTypeInfo filterByName
  matches <- mapM (getMatches showE) filterTypeInfo
  return $
    CaseE
      (VarE filterBy)
      matches
  where
    getMatches showE (pname, cnames) = do
      wildCNames <- mapM getWildNames cnames
      let wildCPatterns = map (second VarP) wildCNames
      let pattern = RecP (mkName $ nameBase pname) wildCPatterns
      let body = NormalB $ UInfixE (LitE (StringL $ "" <> nameBase pname)) (VarE $ mkName "++") (showWildNames showE wildCNames)
      return $ Match pattern body []

handleGetAllKeys :: Name -> Name -> Q Exp
handleGetAllKeys tableRecordName filterByName = do
  tableRecordInfos <- getTypeInfo tableRecordName
  filterTypeInfos <- getTypeInfo filterByName
  when (length tableRecordInfos /= 1) $ fail "More than one Constructors found in table"
  let tableRecord = mkName "_tableRecord"
      showE = mkName "show"
      tableRecordInfo = head tableRecordInfos
  matches <- mapM (\f -> handleGetAllKeysHelper tableRecord tableRecordInfo f showE) filterTypeInfos
  return $ ListE matches
  where
    handleGetAllKeysHelper tableRecord tableRecordInfo filterTypeInfo showE = do
      let (fname, _) = filterTypeInfo
      let (tname, _) = tableRecordInfo
      let cons = getFilterByConsInTableRecord tableRecordInfo filterTypeInfo
      if null cons
        then do
          return $ LitE $ StringL ""
        else do
          matches <- getMatches showE (tname, cons) fname
          return $
            CaseE
              (VarE tableRecord)
              [matches]
    getFilterByConsInTableRecord (_, tCons) (_, fCons) = do
      let cons = intersectBy ((==) `on` fst) fCons tCons
      if length cons == length fCons then cons else []
    getMatches showE (pname, cnames) fname = do
      wildCNames <- mapM getWildNames cnames
      let wildCPatterns = map (second VarP) wildCNames
      let pattern = RecP pname wildCPatterns
      let body = NormalB $ UInfixE (LitE (StringL $ "" <> nameBase fname)) (VarE $ mkName "++") (showWildNames showE wildCNames)
      return $ Match pattern body []

showWildNames :: Name -> [(a, Name)] -> Exp
showWildNames showE wildCNames = do
  let lShowWildNames = map (\(_, n) -> AppE (VarE showE) (VarE n)) wildCNames
  foldr
    (\curr prev -> UInfixE (UInfixE (LitE $ StringL "-") (VarE $ mkName "++") curr) (VarE $ mkName "++") prev)
    (LitE (StringL ""))
    lShowWildNames

getWildNames :: Quote m => (Name, b) -> m (Name, Name)
getWildNames (name, _) = do
  rName <- newName (nameBase name)
  return (mkName $ nameBase name, rName)

deriveCacheClass :: Name -> Name -> Q [Dec]
deriveCacheClass tableRecordName filterByName = do
  [d|
    instance CacheClass $(conT tableRecordName) $(conT filterByName) where
      getAllKeys _tableRecord Proxy = filter (not . null) $(handleGetAllKeys tableRecordName filterByName)
      getKey Proxy _filterBy = $(handleGetKey filterByName)
    |]

handleGetDefaultCache :: Name -> Q Exp
handleGetDefaultCache cacheName = do
  cacheTypeInfo <- getTypeInfo cacheName
  getDefaultCacheHelper cacheTypeInfo
  where
    getDefaultCacheHelper [(pname, cnames)] = do
      UInfixE (ConE pname) (VarE $ mkName "<$>") <$> getCnames cnames
    getDefaultCacheHelper _ = fail "More than one constructor for Cache"
    getCnames (_ : rest) = do
      let iorefs = map (\_ -> AppE (VarE $ mkName "newIORef") (VarE $ mkName "mempty")) rest
      return $ foldr (\c acc -> UInfixE c (VarE $ mkName "<*>") acc) (AppE (VarE $ mkName "newIORef") (VarE $ mkName "mempty")) iorefs
    getCnames _ = fail "No constructors"

handleStartCacheWorkers :: Name -> Name -> Q Exp
handleStartCacheWorkers cacheName cacheChannelName = do
  let cache = mkName "_cache"
      cacheEnabled = mkName "_cacheEnabled"
      cacheStrategy = mkName "_cacheStrategy"
      cacheWorkerFunction = mkName "startCacheWorker"
  cacheTypeInfo <- getTypeInfo cacheName
  startTableCacheWorker cacheTypeInfo cache cacheEnabled cacheStrategy cacheWorkerFunction
  where
    startTableCacheWorker [(_, cacheCNames)] cache cacheEnabled cacheStrategy cacheWorkerFunction = do
      cacheChannelTypeInfo <- getTypeInfo cacheChannelName
      let [(channelPName, channelCNames)] = cacheChannelTypeInfo
      let cacheChannels = map (\(cname, _) -> (nameBase cname, startTableCacheWorkerHelper cname cache cacheEnabled cacheStrategy cacheWorkerFunction)) cacheCNames
      UInfixE (ConE channelPName) (VarE $ mkName "<$>") <$> getCnames channelCNames cacheChannels
    startTableCacheWorker _ _ _ _ _ = fail "More than one constructor for Cache"
    getCnames (fname : rest) cacheChannels = do
      fname' <- getEachValue (fst fname) cacheChannels
      foldM
        ( \acc c -> do
            n <- getEachValue (fst c) cacheChannels
            return $ UInfixE acc (VarE $ mkName "<*>") n
        )
        fname'
        rest
    getCnames _ _ = fail "Illegal Constructor in CacheChannel"
    getEachValue name cacheChannels = do
      case lookup (nameBase name) cacheChannels of
        Just f -> return f
        Nothing -> fail "Illegal Constructor in CacheChannel"
    startTableCacheWorkerHelper cname cache cacheEnabled cacheStrategy cacheWorkerFunction = do
      let newCname = drop 1 $ show cname
      let tableCache =  UInfixE (VarE cache) (VarE $ mkName "^.") (VarE $ mkName $ "C." <> newCname)
      let tableCacheEnabled = UInfixE (VarE cacheEnabled) (VarE $ mkName "^.") (VarE $ mkName $ "C." <> newCname)
      let tableCacheStrategy = UInfixE (VarE cacheStrategy) (VarE $ mkName "^.") (VarE $ mkName $ "C." <> newCname)
      let tableMaxQueueSize = LitE $ IntegerL 1000
      let tableMaxLRUSize = LitE $ IntegerL 1000
      AppE (AppE (AppE (AppE (AppE (VarE cacheWorkerFunction) tableCache) tableCacheEnabled) tableCacheStrategy) tableMaxQueueSize) tableMaxLRUSize

deriveCacheConfig :: Name -> Name -> Name -> Name -> Q [Dec]
deriveCacheConfig cache cacheEnabled cacheStrategy cacheChannel = do
  [d|
    instance CacheConfig $(conT cache) $(conT cacheEnabled) $(conT cacheStrategy) $(conT cacheChannel) where
      getDefaultCache Proxy Proxy Proxy Proxy = $(handleGetDefaultCache cache)
      startCacheWorkers _cache _cacheEnabled _cacheStrategy Proxy = $(handleStartCacheWorkers cache cacheChannel)
    |]
