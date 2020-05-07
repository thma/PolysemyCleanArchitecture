{-# LANGUAGE TemplateHaskell #-}
module UseCases.KVS 
  ( KVS (..)
  , listAllKvs
  , getKvs
  , insertKvs
  , deleteKvs
  )
where

import Polysemy

-- | a key value store specified in terms of Polysemy
data KVS k v m a where
  ListAllKvs :: KVS k v m [(k, v)]
  GetKvs     :: k -> KVS k v m (Maybe v)
  InsertKvs  :: k -> v -> KVS k v m ()
  DeleteKvs  :: k -> KVS k v m ()

makeSem ''KVS

