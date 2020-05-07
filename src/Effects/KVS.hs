{-# LANGUAGE TemplateHaskell #-}
module Effects.KVS 
  ( KVS (..)
  , listAllKvs
  , getKvs
  , insertKvs
  , deleteKvs
  , runKvsOnMapState
  , runKvsPure
  )
where

import Control.Monad
import Polysemy
import Polysemy.State
import qualified Data.Map.Strict as M

-- | a key value store specified in terms of Polysemy
data KVS k v m a where
  ListAllKvs :: KVS k v m [(k, v)]
  GetKvs :: k -> KVS k v m (Maybe v)
  InsertKvs :: k -> v -> KVS k v m ()
  DeleteKvs :: k -> KVS k v m ()

makeSem ''KVS

-- | InMemory implementation of key value store
runKvsOnMapState :: ( Member (State (M.Map k v)) r, Ord k) 
                 => Sem (KVS k v : r) a 
                 -> Sem r a
runKvsOnMapState = interpret $ \case
  ListAllKvs    -> fmap M.toList get
  GetKvs k      -> fmap (M.lookup k) get
  InsertKvs k v -> modify $ M.insert k v
  DeleteKvs k   -> modify $ M.delete k

runKvsPure :: Ord k 
           => M.Map k v
           -> Sem (KVS k v : State (M.Map k v) : r) a 
           -> Sem r (M.Map k v, a)
runKvsPure map = runState map . runKvsOnMapState
