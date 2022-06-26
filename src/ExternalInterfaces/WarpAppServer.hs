module ExternalInterfaces.WarpAppServer where

--import           ExternalInterfaces.AppServer
import           Polysemy (Embed, Sem, Member, embed )
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp               (run)

serveAppWithWarp :: Member (Embed IO) r => Int -> Application -> Sem r ()
serveAppWithWarp port app =
    embed $ run port app