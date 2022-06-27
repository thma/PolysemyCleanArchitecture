module ExternalInterfaces.WarpAppServer where

import           ExternalInterfaces.AppServer
import           Polysemy (Embed, Sem, Member, embed, interpret, runM )
import qualified Network.Wai.Handler.Warp as Warp (run)
import           InterfaceAdapters.Config                 (Config(..))
import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)

    
-- | Warp Based implementation of AppServer
runWarpAppServer :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runWarpAppServer = interpret $ \case
  ServeApp port app         -> embed $ Warp.run port app
  ServeAppFromConfig config -> embed $ 
        let p   = port config
            app = createApp config
        in  Warp.run p app
