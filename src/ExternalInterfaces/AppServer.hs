{-# LANGUAGE TemplateHaskell #-}
module ExternalInterfaces.AppServer where

import Polysemy ( makeSem )
import Network.Wai (Application)
import InterfaceAdapters.Config


data AppServer m a where
  ServeApp           :: Int -> Application -> AppServer m ()
  ServeAppFromConfig :: Config -> AppServer m ()

makeSem ''AppServer



