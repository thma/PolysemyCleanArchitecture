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

newtype KeyValue k v = KeyValue (Map.Map k v) deriving Typeable

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
