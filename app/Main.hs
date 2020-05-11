module Main where

import           Control.Monad.Except
import           Data.Aeson.Types                 (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8       (pack)
import           Data.Function                    ((&))
import qualified Data.Map.Strict                  as M
import qualified Data.Map.Strict                  as M
import           Data.Time.Calendar
import           External.ReservationRestService
import           InterfacesAdapters.Config
import           InterfacesAdapters.KVSFileServer (runKvsAsFileServer)
import           InterfacesAdapters.KVSSqlite     (runKvsAsSQLite)
import qualified Network.Wai.Handler.Warp         as Warp
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                   (Input, runInputConst)
import           Polysemy.Trace                   (Trace, traceToIO)
import           Servant.Server
import           UseCases.KVS                     (KVS)
import           UseCases.ReservationUseCase


--selectBackend  :: Config -> (Show k, Read k, ToJSON v, FromJSON v) => Sem [KVS k v, Input Config, Error ReservationError, Trace, Embed IO] a -> Sem [Input Config, Error ReservationError, Trace, Embed IO] a
selectBackend config = case backend config of
  SQLite     -> runKvsAsSQLite
  FileServer -> runKvsAsFileServer
  

-- | creates the WAI Application that can be executed by Warp.run.
-- All Polysemy interpretations must be executed here.
createApp :: Config -> IO Application
createApp config = do
  return (serve reservationAPI $ hoistServer reservationAPI (interpretServer config) reservationServer)
  where
    interpretServer config sem  =  sem
      & selectBackend config
      & runInputConst config
      & runError @ReservationError
      & traceToIO
      & runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value

main :: IO ()
main = do
  let config = Config {port = 8080, backend = SQLite, dbPath = "kvs.db"} -- In real life this will be external configuration like commandline parameters or a configuration file.
  app  <- createApp config
  putStrLn $ "Starting server on port " ++ show (port config)
  Warp.run (port config) app
