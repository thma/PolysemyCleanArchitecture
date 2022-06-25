{-# LANGUAGE TemplateHaskell #-}
module InterfaceAdapters.AppServer where

import Polysemy ( makeSem )
import Network.Wai (Application)

data AppServer m a where
    ServeApp :: Int -> Application -> AppServer m ()


-- | makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
-- serveApp :: Member (KVS k v) r => Sem r [(k, v)]
-- traceToStdout :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
makeSem ''AppServer