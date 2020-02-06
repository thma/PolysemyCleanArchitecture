{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.DatasourceFileImpl (
  writeToStorage,
  getFromStorage
) where

import           Persistence.Datasource 
import           System.Directory (listDirectory, removeFile, doesFileExist)
import qualified System.IO.Strict as Strict


-- | implements a simple file based storage of the datasource API
instance (Read t, Show t, Monoid t) => Datasource IO t where
  
  writeToStorage entity name = do
    let fileName = fileNameFor name
    writeFile fileName (show entity)

  getFromStorage name = do
    let fileName = fileNameFor name
    fileExists <- doesFileExist fileName
    if fileExists
      then parseFromTxtFile fileName
      else return mempty

-- | create valid file name for a given name
fileNameFor :: String -> FilePath
fileNameFor name = name ++ ".data"

-- | read String from file fileName and then parse the contents as a Read instance.
parseFromTxtFile :: Read a => FilePath -> IO a
parseFromTxtFile fileName = do
  contentString <- Strict.readFile fileName
  return $ read contentString