module ExternalInterfaces.WarpAppServer where

import           ExternalInterfaces.AppServer
import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config               (Config (..))
import qualified Network.Wai.Handler.Warp               as Warp (run)
import           Polysemy                               (Embed, Member, Sem,
                                                         embed, interpret, runM)

-- | Warp Based implementation of AppServer
runWarpAppServer :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runWarpAppServer = interpret $ \case
  ServeApp port app -> embed $ Warp.run port app
  ServeAppFromConfig config ->
    embed $
      let p = port config
          app = createApp config
       in Warp.run p app
