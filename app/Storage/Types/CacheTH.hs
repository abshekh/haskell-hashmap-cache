{-# LANGUAGE TemplateHaskell #-}

module Storage.Types.CacheTH where

import Control.Arrow (Arrow (second))
import Control.Monad (unless)
import Data.Data (Proxy (Proxy))
-- import Data.Generics

import GHC.Exts (IsList (fromList))
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Storage.Types.CacheClass

--
-- normalizeConstructor :: ConstructorInfo -> Q (Name, [(Name, Type)])
-- normalizeConstructor con = do
--   fieldNames <- case constructorVariant con of
--     RecordConstructor xs -> return xs
--     _otherCons -> fail $ show _otherCons <> " found for " <> nameBase (constructorName con)
--   return (constructorName con, zip fieldNames (constructorFields con))

normalizeConstructor :: Con -> Q (Name, [(Name, Type)])
normalizeConstructor con = do
  case con of
    RecC conName fieldTypes -> return (conName, map (\ (n, _, t) -> (n, t)) fieldTypes)
    _otherCons -> fail $ "Invalid constructor found: " <> show _otherCons

getTypeSynInfo :: Dec -> Q [(Name, [(Name, Type)])]
getTypeSynInfo (TySynD name _ typeS) = do
  case typeS of
    AppT (ConT typeF) typeP -> getTypeInfo typeF
    -- AppT (ConT typeF) typeP -> do
    --   (conF, fieldInfoF) <- getTypeInfo typeF
    --   return (conF, map (mapTypeInfo typeP) fieldInfoF)
    _otherSyn -> fail $ "Unexpected type synonym found for type " <> nameBase name <> ": " <> show _otherSyn
  where
  mapTypeInfo :: Type -> (Name, Type) -> (Name, Type)
  mapTypeInfo (ConT nameP) (nameF, AppT (AppT (ConT maybeC) (VarT _)) typeF)
    | nameBase nameP == "Identity" && nameBase maybeC == "C" = (nameF, typeF)
  mapTypeInfo _ nameTypeF = nameTypeF

getTypeInfo :: Name -> Q [(Name, [(Name, Type)])]
getTypeInfo dType = do
  info <- reify dType
  case info of
   TyConI dec ->
    case dec of
      DataD _ _ _ _ consL _ -> mapM normalizeConstructor consL
      NewtypeD _ _ _ _ con _ ->  sequence [normalizeConstructor con]
      type'@TySynD {} -> getTypeSynInfo type'
      _otherDec -> fail $ nameBase dType <> " is not a DateD, NewTypeD or TypeDataD"
   _otherInfo -> fail $ nameBase dType <> " is not a type definition"

-- getTypeInfo :: Name -> Q [(Name, [(Name, Type)])]
-- getTypeInfo dType = do
--   info <- reifyDatatype dType
--   let consL = datatypeCons info
--   mapM normalizeConstructor consL

-- handleToStringHelper :: Name -> Q Exp
-- handleToStringHelper name = do
--   info <- reifyDatatype name
--   let consL = datatypeCons info
--   allCons <- mapM normalizeConstructor consL
--   let allNames' = map stringifyHelper allCons
--       allNames = show allNames'
--   return $ LitE $ StringL allNames
--   where
--     stringifyHelper (pname, snames) = do
--       let allNames' = map (\(n, _) -> nameBase n) snames
--           allNames = nameBase pname : allNames'
--       allNames
--
handleGetKey :: Name -> Q Exp
handleGetKey filterByName = do
  let filterBy = mkName "filterBy"
      showE = mkName "show"
  filterTypeInfo <- getTypeInfo filterByName
  matches <- mapM (getMatches showE) filterTypeInfo
  return $
    CaseE
      (VarE filterBy)
      matches

getMatches showE (pname, cnames) = do
  wildCNames <- mapM getWildNames cnames
  let wildCPatterns = map (second VarP) wildCNames
  let pattern = RecP (mkName $ nameBase pname) wildCPatterns
  let body = NormalB $ UInfixE (LitE (StringL $ "" <> nameBase pname)) (VarE $ mkName "++") (showWildNames showE wildCNames)
  return $ Match pattern body []

getWildNames (name, _) = do
  rName <- newName (nameBase name)
  return (mkName $ nameBase name, rName)

showWildNames showE wildCNames = do
  let lShowWildNames = map (\(_, n) -> AppE (VarE showE) (VarE n)) wildCNames
  foldr
    (\curr prev -> UInfixE (UInfixE (LitE $ StringL "-") (VarE $ mkName "++") curr) (VarE $ mkName "++") prev)
    (LitE (StringL ""))
    lShowWildNames

handleGetAllKeys :: Name -> Name -> Q Exp
handleGetAllKeys tableRecordName filterByName = do
  let tableRecord = mkName "tableRecord"
      showE = mkName "show"
  tableRecordInfo <- getTypeInfo tableRecordName
  filterTypeInfo <- getTypeInfo filterByName
  ListE <$> step1 tableRecord tableRecordInfo filterTypeInfo showE
  where
    -- hashmap _ [] = fail "no cons in tablerecord"
    -- hashmap a [x] = do
    --   let (name, names) = x
    --   lookup a names
    -- hashmap _ (x : xs) = fail "too many cons in tablerecord"
    step1 tableRecord tableRecordInfo filterTypeInfos showE = do
      mapM (\f -> auxStep1 tableRecord tableRecordInfo f showE) filterTypeInfos
    auxStep1 tableRecord tableRecordInfo filterTypeInfo showE = do
      let (fname, rest') = filterTypeInfo
      let [(name', _)] = tableRecordInfo
      matches <- mapM (\t -> getMatches' showE t (mkName $ nameBase fname)) [(name', rest')]
      -- matches <- mapM (\ t -> getMatches' showE t fname) tableRecordInfo
      return $
        CaseE
          (VarE tableRecord)
          matches

getMatches' showE (pname, cnames) fname = do
  wildCNames <- mapM getWildNames cnames
  let wildCPatterns = map (second VarP) wildCNames
  let pattern = RecP (mkName $ nameBase pname) wildCPatterns
  let body = NormalB $ UInfixE (LitE (StringL $ "" <> nameBase fname)) (VarE $ mkName "++") (showWildNames' showE wildCNames)
  return $ Match pattern body []

showWildNames' showE wildCNames = do
  let lShowWildNames = map (\(_, n) -> AppE (VarE showE) (VarE n)) wildCNames
  foldr
    (\curr prev -> UInfixE (UInfixE (LitE $ StringL "-") (VarE $ mkName "++") curr) (VarE $ mkName "++") prev)
    (LitE (StringL ""))
    lShowWildNames

deriveCacheClass :: Name -> Name -> Q [Dec]
deriveCacheClass tableRecordName filterByName = do
  [d|
    instance CacheClass $(conT tableRecordName) $(conT filterByName) where
      getAllKeys tableRecord Proxy = $(handleGetAllKeys tableRecordName filterByName)
      getKey Proxy filterBy = $(handleGetKey filterByName)
    |]
