module ExternalInterfaces.HalAppServer where

import           ExternalInterfaces.AppServer
import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config               (Config (..))
import           AWS.Lambda.Runtime (mRuntime)
import qualified Network.Wai.Handler.Hal as WaiHandler  (run)
import           Polysemy                               (Embed, Member, Sem,
                                                         embed, interpret, runM)

-- | AWS Lambda HAL Based implementation of AppServer
runHalAppServer :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runHalAppServer = interpret $ \case
  ServeApp app -> embed $ mRuntime $ WaiHandler.run app
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in mRuntime $ WaiHandler.run app
