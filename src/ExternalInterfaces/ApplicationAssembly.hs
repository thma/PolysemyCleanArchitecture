module ExternalInterfaces.ApplicationAssembly where

import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8               (pack)
import           Data.Function                            ((&))
import           Data.IORef                               (newIORef)
import qualified Data.Map.Strict                          as M
import           Data.Time.Calendar
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSFileServer          (runKvsAsFileServer)
import           InterfaceAdapters.KVSSqlite              (runKvsAsSQLite)
import           InterfaceAdapters.ReservationRestService
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                           (Input, runInputConst)
import           Polysemy.Trace                           (Trace, traceToIO)
import           Servant.Server
import           UseCases.ReservationUseCase



-- | creates the WAI Application that can be executed by Warp.run.
-- All Polysemy interpretations must be executed here.
createApp :: Config -> IO Application
createApp config = do
  kvsIORef <- newIORef (M.fromList [] :: ReservationMap)
  return (serve reservationAPI $ hoistServer reservationAPI (interpretServer config kvsIORef) reservationServer)
  where
    interpretServer config kvsIORef sem  =  sem
      & selectBackend config kvsIORef
      & runInputConst config
      & runError @ReservationError
      & traceToIO
      & runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value

-- | load application config. In real life, this would load a config file or read commandline args.
loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = SQLite, dbPath = "kvs.db"}

-- | can select between SQLite or FileServer persistence backends.
selectBackend config kvsIORef = case backend config of
  SQLite     -> runKvsAsSQLite
  FileServer -> runKvsAsFileServer
  InMemory   -> error "not supported"