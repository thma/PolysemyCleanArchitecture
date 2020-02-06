{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
module Persistence.ReservationTablePersistence where

import           Persistence.Datasource      (Datasource, getFromStorage, writeToStorage)
import           BL.ReservationBusinessLogic (ReservationTable)

-- | load a table from persistent storage
loadTable :: (Datasource m ReservationTable) => String -> m ReservationTable
loadTable = getFromStorage

-- | store a table to persistent storage
storeTable :: (Datasource m ReservationTable) => m ReservationTable -> String -> m ()
storeTable table name = do 
  tab <- table
  writeToStorage tab name
