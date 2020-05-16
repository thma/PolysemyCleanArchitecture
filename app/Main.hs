module Main where

import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config
import           Network.Wai.Handler.Warp               (run)

main :: IO ()
main = do
  config <- loadConfig
  app    <- createApp config
  putStrLn $ "Starting server on port " ++ show (port config)
  run (port config) app
