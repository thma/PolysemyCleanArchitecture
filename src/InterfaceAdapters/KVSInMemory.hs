module InterfaceAdapters.KVSInMemory 
  ( runKvsPure
  , runKvsOnMapState
  ) 
where

import Polysemy
import Polysemy.State
import qualified Data.Map.Strict as M

import UseCases.KVS

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
runKvsPure kvMap = runState kvMap . runKvsOnMapState