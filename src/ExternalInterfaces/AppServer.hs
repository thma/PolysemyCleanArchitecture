{-# LANGUAGE TemplateHaskell #-}

module ExternalInterfaces.AppServer where

import           InterfaceAdapters.Config
import           Network.Wai              (Application)
import           Polysemy                 (makeSem)

data AppServer m a where
  ServeApp :: Int -> Application -> AppServer m ()
  ServeAppFromConfig :: Config -> AppServer m ()

makeSem ''AppServer
