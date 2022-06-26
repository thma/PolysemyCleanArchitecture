{-# LANGUAGE TemplateHaskell #-}
module ExternalInterfaces.AppServer where

import Polysemy ( makeSem )
import Network.Wai (Application)


data AppServer m a where
  ServeApp :: Int -> Application -> AppServer m ()


makeSem ''AppServer



