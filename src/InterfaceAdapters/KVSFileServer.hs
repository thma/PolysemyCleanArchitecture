module InterfaceAdapters.KVSFileServer
  ( runKvsAsFileServer
  , dataDir
  ) 
where

import           Control.Exception
import           Data.Aeson        (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import           Data.List         (isSuffixOf)
import           UseCases.KVS   (KVS (..))
import           Polysemy
import           System.Directory  (doesFileExist, listDirectory, removeFile)


-- | File Based implementation of key value store
runKvsAsFileServer :: (Member (Embed IO) r, Show k, Read k, ToJSON v, FromJSON v) => Sem (KVS k v : r) a -> Sem r a
runKvsAsFileServer = interpret $ \case
  ListAllKvs        -> embed retrieveAll
  GetKvs key        -> embed (getAction key)
  InsertKvs key val -> embed (storeEntity (show key) val)
  DeleteKvs key     -> embed (removeFile (show key))

getAction :: (Show k, FromJSON v) => k -> IO (Maybe v)
getAction key = do
  let fileName = getPath (show key)
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      x <- retrieveEntity (show key)
      return (Just x)
    else return Nothing

-- | load persistent entity of type a and identified by id from the filesystem
retrieveEntity :: (FromJSON a) => String -> IO a
retrieveEntity = decodeFile . getPath

-- | store persistent entity of type a and identified by id to the filesystem
storeEntity :: (ToJSON a) => String -> a -> IO ()
storeEntity key = encodeFile (getPath key)

-- | compute path of data file
getPath :: String -> String
getPath file = dataDir ++ file ++ extension

dataDir :: FilePath
dataDir   = ".stack-work/"

extension :: [Char]
extension = ".json"

-- | load all persistent entities of type a
retrieveAll :: (FromJSON a, Read k) => IO [(k, a)]
retrieveAll = do
  allFiles <- listDirectory dataDir
  let filteredFiles = filter (isSuffixOf extension) allFiles
  resList <- mapM (\fname -> decodeFile  (dataDir ++ fname)) filteredFiles
  return $ zip (map (read . reverse . drop (length extension) . reverse) filteredFiles) resList

-- | parse an entity from a json file
decodeFile :: FromJSON a => String -> IO a
decodeFile jsonFileName= do
  eitherEntity <- eitherDecodeFileStrict jsonFileName
  case eitherEntity of
    Left msg -> throw (InternalError $ "could not parse data: " ++ msg)
    Right e  -> return e


-- | exeptions that may occur during persistence operations
data PersistenceException = EntityNotFound String
    | EntityAlreadyExists String
    | InternalError String
    deriving (Show)

instance Exception PersistenceException