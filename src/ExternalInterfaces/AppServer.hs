{-# LANGUAGE TemplateHaskell #-}

module ExternalInterfaces.AppServer where

import           InterfaceAdapters.Config
import           Network.Wai              (Application)
import           Polysemy                 (makeSem)

{-
  This module defines an AppServer effect that allows to host a WAI Application.
-}
data AppServer m a where
  -- ^ serve a WAI Application
  ServeApp           :: Application -> AppServer m ()
  
  -- ^ create a WAI Application from Config and then serve it
  ServeAppFromConfig :: Config -> AppServer m ()

makeSem ''AppServer
