{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE StandaloneDeriving  #-}
-- {-# OPTIONS_GHC -ddump-splices  #-}

module InterfaceAdapters.KVSAcidState
  ( runKvsAsAcidState,
  )
where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import qualified Data.Map             as Map
import           Data.SafeCopy
import           Data.Typeable
import           Polysemy
import           UseCases.KVS         (KVS (..))
--import           Data.Acid.Advanced

newtype KeyValue k v = KeyValue (Map.Map k v) deriving Typeable

-- $(deriveSafeCopy 0 'base ''KeyValue)
instance (Ord k, SafeCopy k, Typeable k, SafeCopy v, Typeable v) => SafeCopy (KeyValue k v) where
    putCopy (KeyValue state) = contain $ safePut state
    getCopy = contain $ fmap KeyValue safeGet


insertKey :: (Ord k) => k -> v -> Update (KeyValue k v) ()
insertKey key value = do
  KeyValue m <- get
  put (KeyValue (Map.insert key value m))

deleteKey :: (Ord k) => k -> v -> Update (KeyValue k v) ()
deleteKey key _ = do
  KeyValue m <- get
  put (KeyValue (Map.delete key m))

lookupKey :: (Ord k) => k -> Query (KeyValue k v) (Maybe v)
lookupKey key = do
  KeyValue m <- ask
  return (Map.lookup key m)

listAll :: Query (KeyValue k v) (KeyValue k v)
listAll = do ask

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey, 'listAll, 'deleteKey])

---------------------

openDB :: (SafeCopy k, Typeable k, Ord k, SafeCopy v, Typeable v) => IO (AcidState (KeyValue k v))
openDB = openLocalStateFrom "_state" (KeyValue Map.empty)

-- | AcidState based implementation of key value store
runKvsAsAcidState :: (Member (Embed IO) r, Typeable k, SafeCopy k, Ord k, Typeable v, SafeCopy v) 
  => Sem (KVS k v : r) a -> Sem r a
runKvsAsAcidState = interpret $ \case
  ListAllKvs -> embed retrieveAll
  GetKvs key -> embed (getAction key)
  InsertKvs key val -> embed (insertAction key val)
  DeleteKvs key -> undefined --embed (deleteAction key)

retrieveAll :: (Typeable k, SafeCopy k, Ord k, Typeable v, SafeCopy v) => IO [(k, v)]
retrieveAll = do
  acid <- openDB
  (KeyValue m) <- query acid ListAll
  closeAcidState acid
  return $ Map.assocs m

getAction :: (Typeable k, SafeCopy k, Ord k, Typeable v, SafeCopy v) => k -> IO (Maybe v)
getAction key = do
  acid <- openDB
  let result = query acid (LookupKey key )
  closeAcidState acid
  result

insertAction :: (Typeable k, SafeCopy k, Ord k, Typeable v, SafeCopy v) => k -> v -> IO ()
insertAction key val = do
  acid <- openDB
  update acid (InsertKey key val)
  closeAcidState acid

--deleteAction :: (Typeable k, SafeCopy k, Ord k, Typeable v, SafeCopy v) => k -> IO [v]
--deleteAction key = undefined
--  do
--  acid <- openDB
--  update acid (DeleteKey key)
--  closeAcidState acid
--  return []


------------------------------------------------------
-- The gritty details. These things may be done with
-- Template Haskell in the future.

--data InsertKey k v = InsertKey k v
--newtype LookupKey k v = ConLookupKey k -- v
--
--
--deriving instance Typeable InsertKey
--instance (Typeable k, SafeCopy k, Typeable v, SafeCopy v) => SafeCopy (InsertKey k v) where
--    putCopy (InsertKey key value) = contain $ safePut key >> safePut value
--    getCopy = contain $ InsertKey <$> safeGet <*> safeGet
--instance (Typeable k, Typeable v) => Method (InsertKey k v) where
--    type MethodResult (InsertKey k v) = ()
--    type MethodState (InsertKey k v) = KeyValue k v
--instance (Typeable k, Typeable v) => UpdateEvent (InsertKey k v)
--
--deriving instance Typeable LookupKey
--instance (Typeable k, SafeCopy k, Typeable v, SafeCopy v) => SafeCopy (LookupKey k v) where
--    putCopy (ConLookupKey key) = contain $ safePut key
--    getCopy = contain $ ConLookupKey <$> safeGet -- <*> safeGet
--instance (Typeable k, SafeCopy k, Typeable v, SafeCopy v) => Method (LookupKey k v) where
--    type MethodResult (LookupKey k v) = Maybe v
--    type MethodState (LookupKey k v) = KeyValue k v
--instance (Typeable k, SafeCopy k, Typeable v, SafeCopy v) => QueryEvent (LookupKey k v)
--
--instance (Typeable k, SafeCopy k, Ord k, Typeable v, SafeCopy v) => IsAcidic (KeyValue k v) where
--    acidEvents = 
--      [ UpdateEvent (\(InsertKey key value) -> insertKey key value) safeCopyMethodSerialiser
--      , (QueryEvent (\(ConLookupKey key)   -> lookupKey key))       safeCopyMethodSerialiser
--      ]

{--
deleteUser :: Id -> Handler ()
deleteUser id = do
  liftIO $ putStrLn $ "DELETE /users/" ++ id
  eitherVoidEx <- liftIO $ try (delete userType id) :: Handler (Either PersistenceException ())
  case eitherVoidEx of
    Left ex -> throwAsServerError ex
    Right v -> return v
  where
    userType = Proxy :: Proxy User
    
-- | delete an entity of type a and identified by an Id to a json file
delete :: Proxy a -> Id -> IO ()
delete proxy id = do
    -- compute file path based on runtime type and entity id
    let jsonFileName = getPath (typeRep proxy) id
    fileExists <- doesFileExist jsonFileName
    if fileExists
      then removeFile jsonFileName
      else throw $ EntityNotFound ("could not delete as entity was not found: " ++ jsonFileName)
    

--}