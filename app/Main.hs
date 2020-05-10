module Main where

import           Data.Function ((&))

import qualified Network.Wai.Handler.Warp as Warp
import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace (Trace, traceToIO)
import           Polysemy.Input (Input, runInputConst)
import           Data.Aeson.Types (ToJSON, FromJSON)
import           Servant.Server
import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.Time.Calendar
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map.Strict                    as M

import           UseCases.KVS               (KVS)
import           InterfacesAdapters.KVSFileServer (runKvsAsFileServer)
import           InterfacesAdapters.KVSSqlite (runKvsAsSQLite)
import           External.ReservationRestService
import           UseCases.ReservationUseCase
import           UseCases.Config


selectBackend  :: Config -> (Show k, Read k, ToJSON v, FromJSON v) => Sem [KVS k v, Input Config, Error ReservationError, Trace, Embed IO] a -> Sem [Input Config, Error ReservationError, Trace, Embed IO] a
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
  let config = Config {maxCapacity = 20, port = 8080, backend = SQLite, dbPath = "kvs.db"} -- In real life this will be external configuration like commandline parameters or a configuration file.
  app  <- createApp config
  putStrLn $ "Starting server on port " ++ show (port config)
  Warp.run (port config) app
