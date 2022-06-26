module Main where

import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config
import           Network.Wai.Handler.Warp               (run)
import           SwaggerUI (swagger)
import           ExternalInterfaces.WarpAppServer
import           Data.Function                            ((&))
import           Polysemy                                 (runM)

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

simpleMain' :: IO ()
simpleMain' = do
  config <- loadConfig
  let p = port config
      app = createApp config
  serveAppWithWarp p app 
    & runM    