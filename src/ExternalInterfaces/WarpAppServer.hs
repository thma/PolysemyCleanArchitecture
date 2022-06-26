module ExternalInterfaces.WarpAppServer where

import           ExternalInterfaces.AppServer
import           Polysemy (Embed, Sem, Member, embed, interpret )
import qualified Network.Wai.Handler.Warp as Warp (run)

    
-- | Warp Based implementation of AppServer
runAppServerOnWarp :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runAppServerOnWarp = interpret $ \case
  ServeApp port app -> embed $ Warp.run port app
