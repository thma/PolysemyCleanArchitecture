{-# LANGUAGE TemplateHaskell #-}
module BL.KVS where

import Control.Monad
import Polysemy
import Polysemy.State
import qualified Data.Map.Strict as M

data KVS k v m a where
  ListAllKvs :: KVS k v m [(k, v)]
  GetKvs :: k -> KVS k v m (Maybe v)
  InsertKvs :: k -> v -> KVS k v m ()
  DeleteKvs :: k -> KVS k v m ()

makeSem ''KVS

runKvsOnMapState :: ( Member (State (M.Map k v)) r
                    , Ord k
                    ) => Sem ((KVS k v) ': r) a -> Sem r a
runKvsOnMapState = interpret $ \case
  ListAllKvs    -> fmap M.toList get
  GetKvs k      -> fmap (M.lookup k) get
  InsertKvs k v -> modify $ M.insert k v
  DeleteKvs k   -> modify $ M.delete k

