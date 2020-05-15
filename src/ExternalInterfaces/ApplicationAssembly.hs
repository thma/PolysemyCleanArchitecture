module ExternalInterfaces.ApplicationAssembly where

import           Control.Monad.Except
--import           Data.Aeson.Types                (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8      (pack)
import           Data.Function                   ((&))
import           Data.IORef                      (newIORef)
import qualified Data.Map.Strict                 as M
--import qualified Data.Map.Strict                 as M
import           Data.Time.Calendar
import           InterfaceAdapters.ReservationRestService
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSFileServer (runKvsAsFileServer)
--import           InterfaceAdapters.KVSInMemory   (runKvsOnMapState)
import           InterfaceAdapters.KVSSqlite     (runKvsAsSQLite)
--import qualified Network.Wai.Handler.Warp        as Warp
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                  (Input, runInputConst)
--import           Polysemy.State                  (runStateIORef)
import           Polysemy.Trace                  (Trace, traceToIO)
import           Servant.Server
--import           UseCases.KVS                    (KVS)
import           UseCases.ReservationUseCase

selectBackend config kvsIORef = case backend config of
  SQLite     -> runKvsAsSQLite
  FileServer -> runKvsAsFileServer

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

