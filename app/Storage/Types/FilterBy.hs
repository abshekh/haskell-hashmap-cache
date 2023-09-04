-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Storage.Types.FilterBy where

import qualified Database.Beam as B
import qualified Database.Beam.Sqlite as Sqlite

class FilterByTrait filterBy table | filterBy -> table, table -> filterBy where
  getKeyName :: filterBy -> String
  getPredicate ::
    filterBy ->
    table (B.QGenExpr B.QValueContext Sqlite.Sqlite s) ->
    B.QGenExpr B.QValueContext Sqlite.Sqlite s B.SqlBool
  -- getX

-- {-# MINIMAL getKeyName, getPredicate #-}
