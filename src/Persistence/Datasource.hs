{-# LANGUAGE MultiParamTypeClasses #-}
module Persistence.Datasource where

-- | Datasource m t defines an abstract interface that allows to store entities of type t in a monadic action in Monad m
class (Monad m, Read t, Show t, Monoid t) => Datasource m t where
  -- | load an entity from the persistent storage
  getFromStorage :: String -> m t
  -- | write entity to persistent storage
  writeToStorage :: t -> String -> m ()

