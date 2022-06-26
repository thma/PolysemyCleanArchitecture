module ExternalInterfaces.ApplicationAssembly where

import           Control.Monad.Except                     (ExceptT(ExceptT))
import           Data.ByteString.Lazy.Char8               (pack)
import           Data.Function                            ((&))
import           InterfaceAdapters.Config                 (Backend(SQLite, FileServer), Config(..))
import           InterfaceAdapters.KVSFileServer          (runKvsAsFileServer)
import           InterfaceAdapters.KVSSqlite              (runKvsAsSQLite)
import           InterfaceAdapters.ReservationRestService (reservationAPI, reservationServer, ReservationAPI)
import           Polysemy                                 (Sem, runM, Embed, Member)
import           Polysemy.Error                           (Error, runError)
import           Polysemy.Input                           (Input, runInputConst)
import           Polysemy.Trace                           (Trace, traceToStdout, ignoreTrace)
import           Servant.Server                           (serve, errBody, err412, Handler(..), ServerT, Application, hoistServer, ServerError)
import           UseCases.KVS                             (KVS)
import           UseCases.ReservationUseCase              (ReservationError(..))
import           Data.Aeson.Types                         (ToJSON, FromJSON)
import           ExternalInterfaces.AppServer (serveApp, AppServer)
import           ExternalInterfaces.WarpAppServer (runAppServerOnWarp)

-- | creates the WAI Application that can be executed by Warp.run.
-- All Polysemy interpretations must be executed here.
createApp :: Config -> Application
createApp config = serve reservationAPI (liftServer config)


liftServer :: Config -> ServerT ReservationAPI Handler
liftServer config = hoistServer reservationAPI (interpretServer config) reservationServer
  where
    interpretServer :: (Show k, Read k, ToJSON v, FromJSON v)
                    => Config -> Sem '[KVS k v, Input Config, Trace, Error ReservationError, Embed IO] a -> Handler a
    interpretServer conf sem  =  sem
          & selectKvsBackend conf
          & runInputConst conf
          & selectTraceVerbosity conf
          & runError @ReservationError
          & runM
          & liftToHandler

    liftToHandler :: IO (Either ReservationError a) -> Handler a
    liftToHandler = Handler . ExceptT . fmap handleErrors

    handleErrors :: Either ReservationError b -> Either ServerError b
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value

-- | can select between SQLite or FileServer persistence backends.
selectKvsBackend :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v)
                 => Config -> Sem (KVS k v : r) a -> Sem r a
selectKvsBackend config = case backend config of
  SQLite     -> runKvsAsSQLite
  FileServer -> runKvsAsFileServer
  
-- | if the config flag verbose is set to True, trace to Console, else ignore all trace messages
selectTraceVerbosity :: (Member (Embed IO) r) => Config -> (Sem (Trace : r) a -> Sem r a)
selectTraceVerbosity config =
  if verbose config
    then traceToStdout
    else ignoreTrace
    
-- | load application config. In real life, this would load a config file or read commandline args.
loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = SQLite, dbPath = "kvs.db", verbose = True}

-- | build app from config and serve with AppServer effect.
serveAppFromConfig :: (Member AppServer r) => Config -> Sem r ()
serveAppFromConfig config = do
  let p   = port config
      app = createApp config
  serveApp p app

-- | in this example the AppServer effect is interpreted by runAppServerOnWarp
sampleMain :: IO ()
sampleMain = do
  config <- loadConfig
  serveAppFromConfig config
    & runAppServerOnWarp    -- use Warp to run rest application
    & runM
