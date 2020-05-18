module ExternalInterfaces.ApplicationAssembly where

import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8               (pack)
import           Data.Function                            ((&))
import qualified Data.Map.Strict                          as M
import           Data.Time.Calendar
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSFileServer          (runKvsAsFileServer)
import           InterfaceAdapters.KVSSqlite              (runKvsAsSQLite)
import           InterfaceAdapters.ReservationRestService
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                           (Input, runInputConst)
import           Polysemy.Trace                           (Trace, traceToIO, ignoreTrace)
import           Servant.Server
import           UseCases.KVS
import           UseCases.ReservationUseCase
import           Data.Aeson.Types (ToJSON, FromJSON)



-- | creates the WAI Application that can be executed by Warp.run.
-- All Polysemy interpretations must be executed here.
createApp :: Config -> Application
createApp config = serve reservationAPI (liftServer config)

liftServer :: Config -> ServerT ReservationAPI Handler
liftServer config = hoistServer reservationAPI (interpretServer config) reservationServer
  where
    interpretServer config sem  =  sem
      & selectKvsBackend config
      & runInputConst config
      & runError @ReservationError
      & selectTraceVerbosity config
      & runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value

-- | can select between SQLite or FileServer persistence backends.
selectKvsBackend :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v)
                 => Config -> Sem (KVS k v : r) a -> Sem r a
selectKvsBackend config = case backend config of
  SQLite     -> runKvsAsSQLite
  FileServer -> runKvsAsFileServer
  InMemory   -> error "not supported"

-- | if the config flag verbose is set to True, trace to Console, else ignore all trace messages
selectTraceVerbosity :: (Member (Embed IO) r) => Config -> (Sem (Trace : r) a -> Sem r a)
selectTraceVerbosity config =
  if verbose config
    then traceToIO
    else ignoreTrace
    
-- | load application config. In real life, this would load a config file or read commandline args.
loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = SQLite, dbPath = "kvs.db", verbose = True}