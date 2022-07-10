{-# LANGUAGE TemplateHaskell #-}
module ExternalInterfaces.Hosting where

import           ExternalInterfaces.ConfigProvider        (ConfigProvider, getConfig)
import           ExternalInterfaces.AppServer             (AppServer(..), serveAppFromConfig)
import           ExternalInterfaces.WarpAppServer         (runWarpAppServer)
import           ExternalInterfaces.HalAppServer          (runHalAppServer)
import           InterfaceAdapters.Config                 (Config(..), Hosting(..))
import           Polysemy
import           Polysemy.Internal                        (Sem (..))



-- | load configuration via ConfigProvider effect, then contruct and run app via AppServer effect
configureAndServeApp ::  ( Member ConfigProvider r, Member AppServer r)  => Sem r ()
configureAndServeApp = do
  config <- getConfig
  serveAppFromConfig config

runSelectedHosting :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runSelectedHosting = undefined

{-runSelectedHosting :: (Member (Embed IO) r) => Sem (AppServer ': r) a -> Sem r a
runSelectedHosting = interpret $ \eff ->
  case eff of 
    ServeAppFromConfig config -> 
      case hosting config of
       Warp -> reinterpret $ runWarpAppServer eff
       Hal  -> reinterpret $ runHalAppServer eff-}




data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg
  

