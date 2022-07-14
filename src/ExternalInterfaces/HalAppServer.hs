module ExternalInterfaces.HalAppServer where

import           ExternalInterfaces.AppServer
import           ExternalInterfaces.ApplicationAssembly (createApp)
import           AWS.Lambda.Runtime                     (mRuntime)
import qualified Network.Wai.Handler.Hal as Hal         (run)
import           Polysemy                               (Embed, Member, Sem, embed, interpret)

-- | AWS Lambda HAL Based implementation of AppServer
runHalAppServer :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runHalAppServer = interpret $ \case
  ServeApp app -> embed $ mRuntime $ Hal.run app
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in mRuntime $ Hal.run app
