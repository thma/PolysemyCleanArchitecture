{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module InterfaceAdapters.KVSAcidState 
  (
    runKvsAsAcidState
  ) where

--import           Control.Exception
import           Data.Aeson        (FromJSON, ToJSON)
--import           Data.List         (isSuffixOf)
import           UseCases.KVS   (KVS (..))
import           Polysemy
--import           System.Directory  (doesFileExist, listDirectory, removeFile)

import           Data.Acid
--import           Data.Acid.Advanced
import qualified Data.Map             as Map
import           Data.Typeable
import           Data.SafeCopy
import           Control.Monad.State
import           Control.Monad.Reader

type Key = String

newtype KeyValue v = KeyValue (Map.Map Key v) deriving Typeable

getAllValues (KeyValue kvMap) = Map.assocs kvMap

$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: Key -> v -> Update (KeyValue v) ()
insertKey key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))
         
lookupKey :: Key -> Query (KeyValue v) (Maybe v)
lookupKey key
    = do KeyValue m <- ask
         return (Map.lookup key m)         

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])


-- | File Based implementation of key value store
runKvsAsAcidState :: (Member (Embed IO) r, Show k, Typeable v, SafeCopy v) => Sem (KVS k v : r) a -> Sem r a
runKvsAsAcidState = interpret $ \case
  ListAllKvs        -> undefined
  GetKvs key        -> embed (getAction (show key))
  InsertKvs key val -> embed (storeEntity (show key) val)
  DeleteKvs key     -> embed (removeFile (show key))

retrieveAll ::  (FromJSON a, Read k) => IO [(k, a)]
retrieveAll = undefined


getAction :: (Typeable v, SafeCopy v) => [Char] -> IO (Maybe v)
getAction key = do
  acid <- openLocalState (KeyValue Map.empty)
  let result = query acid (LookupKey key)
  closeAcidState acid
  result
  

storeEntity :: (Typeable v, SafeCopy v) => [Char] -> v -> IO ()
storeEntity key val = do
  acid <- openLocalState (KeyValue Map.empty)
  update acid (InsertKey key val)
  closeAcidState acid

removeFile = undefined

