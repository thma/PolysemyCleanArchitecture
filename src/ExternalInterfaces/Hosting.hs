module ExternalInterfaces.Hosting where

import           ExternalInterfaces.ConfigProvider        (ConfigProvider, getConfig)
import           ExternalInterfaces.AppServer             (AppServer(..), serveAppFromConfig)
import           ExternalInterfaces.WarpAppServer         (runWarpAppServer)
import           ExternalInterfaces.HalAppServer          (runHalAppServer)
import           InterfaceAdapters.Config                 (Config(..), Hosting(..))
import           Polysemy


-- | load configuration via ConfigProvider effect, then contruct and run app via AppServer effect
configureAndServeApp ::  ( Member ConfigProvider r, Member AppServer r)  => Sem r ()
configureAndServeApp = do
  config <- getConfig
  serveAppFromConfig config


{-runSelectedHosting :: Sem (AppServer : r) a -> Sem r a
runSelectedHosting = interpret $ \case
  ServeAppFromConfig config ->
    case hosting config of
      Warp -> runWarpAppServer
      Hal  -> runHalAppServer-}



