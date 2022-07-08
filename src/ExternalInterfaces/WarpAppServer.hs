module ExternalInterfaces.WarpAppServer where

import           ExternalInterfaces.AppServer
import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config               (Config (..))
import qualified Network.Wai.Handler.Warp               as Warp (run)
import           Polysemy                               (Embed, Member, Sem,
                                                         embed, interpret, runM)

-- | Warp Based implementation of AppServer
runWarpAppServer :: (Member (Embed IO) r) => Int -> Sem (AppServer : r) a -> Sem r a
runWarpAppServer port = interpret $ \case
  -- this is the more generic version which maps directly to Warp.run
  ServeApp app -> embed $ Warp.run port app

  -- serving an application by constructing it from a config
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in do 
        putStrLn $ "starting Warp on Port " ++ show port
        Warp.run port app

runWarpAppServerWithConfigPort :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runWarpAppServerWithConfigPort = interpret $ \case
  -- this is the more generic version which maps directly to Warp.run
  ServeApp app -> embed $ Warp.run 8080 app

  -- serving an application by constructing it from a config
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in do 
        putStrLn $ "starting Warp on Port " ++ show (port config)
        Warp.run (port config) app

