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

-- | a key value store specified as A GADT type
data KVS k v m a where
  ListAllKvs :: KVS k v m [(k, v)]
  GetKvs     :: k -> KVS k v m (Maybe v)
  InsertKvs  :: k -> v -> KVS k v m ()
  DeleteKvs  :: k -> KVS k v m ()

-- | makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
-- listAllKvs :: Member (KVS k v) r => Sem r [(k, v)]
-- getKvs     :: Member (KVS k v) r => k -> Sem r (Maybe v)
-- insertKvs  :: Member (KVS k v) r => k -> v -> Sem r ()
-- deleteKvs  :: Member (KVS k v) r => k -> Sem r ()
makeSem ''KVS

