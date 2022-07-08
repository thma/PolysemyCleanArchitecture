module Main where

import           Data.Function                          ((&))
import           ExternalInterfaces.AppServer           (serveAppFromConfig, AppServer)
import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig, serveConfiguredApp)
import           ExternalInterfaces.WarpAppServer       (runWarpAppServer, runWarpAppServerWithConfigPort)
import           ExternalInterfaces.HalAppServer        (runHalAppServer)
import           InterfaceAdapters.Config
import           Network.Wai.Handler.Warp               (run)
import           Polysemy                               (runM, Sem, Member )
import           SwaggerUI                              (swagger)
import           ExternalInterfaces.ConfigProvider
import           ExternalInterfaces.FileConfigProvider

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

-- | in this example the AppServer effect is interpreted by runWarpAppServer
warpAsEffectMain :: IO ()
warpAsEffectMain = do
  config <- loadConfig
  let p = port config
  serveAppFromConfig config
    & runWarpAppServer p -- use Warp to run rest application
    & runM

-- | in this example the AppServer effect is interpreted by runHalAppServer
halAsEffectMain :: IO ()
halAsEffectMain = do
  config <- loadConfig
  serveAppFromConfig config
    & runHalAppServer -- use HAL to run rest application
    & runM    

-- | This example treats loading of configuration as yet another effect.
test :: IO ()
test = do
  serveConfiguredApp
    & runFileConfigProvider "application.config"  -- provide Config from a file
    & runWarpAppServerWithConfigPort              -- use Warp to run rest application
    & runM
 