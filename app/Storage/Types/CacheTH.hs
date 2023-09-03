{-# LANGUAGE TemplateHaskell #-}

module Storage.Types.CacheTH where

import Control.Arrow (Arrow (second))
import Control.Monad (when)
import Data.Data (Proxy (Proxy))
import Data.Function (on)
import Data.List (intersectBy)
import Language.Haskell.TH
import Storage.Types.CacheClass

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
      matches <- getMatches showE (tname, cons) fname
      return $
        CaseE
          (VarE tableRecord)
          [matches]
    getFilterByConsInTableRecord (_, tCons) (_, fCons) = intersectBy ((==) `on` fst) fCons tCons
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
      getAllKeys _tableRecord Proxy = $(handleGetAllKeys tableRecordName filterByName)
      getKey Proxy _filterBy = $(handleGetKey filterByName)
    |]
