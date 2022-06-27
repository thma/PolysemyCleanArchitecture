module Main where

import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config
import           Network.Wai.Handler.Warp               (run)
import           SwaggerUI (swagger)
import           ExternalInterfaces.AppServer (serveAppFromConfig)
import           ExternalInterfaces.WarpAppServer (runWarpAppServer)
import           Polysemy (runM)
import           Data.Function                            ((&))


-- | the POSH version of the application: REST service + SwaggerUI
main :: IO ()
main = swagger

-- | the poor man's version of the application: just a REST service
simpleMain :: IO ()
simpleMain = do
  config <- loadConfig
  let p = port config
  putStrLn $ "Starting server on port " ++ show p
  run p (createApp config)
  
-- | in this example the AppServer effect is interpreted by runAppServerOnWarp
warpAsEffectMain :: IO ()
warpAsEffectMain = do
  config <- loadConfig
  serveAppFromConfig config
    & runWarpAppServer    -- use Warp to run rest application
    & runM