module ExternalInterfaces.WarpAppServer where

import           ExternalInterfaces.AppServer
import           ExternalInterfaces.ApplicationAssembly (createApp)
import           InterfaceAdapters.Config               (Config (..))
import qualified Network.Wai.Handler.Warp               as Warp (run)
import           Polysemy                               (Embed, Member, Sem,
                                                         embed, interpret)

-- | Warp Based implementation of AppServer
runWarpAppServerOnPort :: (Member (Embed IO) r) => Int -> Sem (AppServer : r) a -> Sem r a
runWarpAppServerOnPort port = interpret $ \case
  -- this is the more generic version which maps directly to Warp.run
  ServeApp app -> embed $ Warp.run port app

  -- serving an application by constructing it from a config
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in do 
        putStrLn $ "starting Warp on Port " ++ show port
        Warp.run port app

runWarpAppServer :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runWarpAppServer = interpret $ \case
  -- this is the more generic version which maps directly to Warp.run
  ServeApp app -> embed $ Warp.run 8080 app

  -- serving an application by constructing it from a config
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in do 
        putStrLn $ "starting Warp on Port " ++ show (port config)
        Warp.run (port config) app

