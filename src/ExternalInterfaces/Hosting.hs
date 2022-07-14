module ExternalInterfaces.Hosting where

import           ExternalInterfaces.ConfigProvider        (ConfigProvider, getConfig)
import           ExternalInterfaces.AppServer             (AppServer(..), serveAppFromConfig)
import           Polysemy




-- | load configuration via ConfigProvider effect, then contruct and run app via AppServer effect
configureAndServeApp ::  ( Member ConfigProvider r, Member AppServer r)  => Sem r ()
configureAndServeApp = do
  config <- getConfig
  serveAppFromConfig config

  

