module Main where

import           Data.Function ((&))
import           InterfacesAdapters.KVSFileServer (runKvsAsFileServer)
import           InterfacesAdapters.KVSSqlite
import qualified Network.Wai.Handler.Warp as Warp
import           Polysemy
import           Polysemy.Error
import           Servant.Server

import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.Time.Calendar
import           Data.ByteString.Lazy.Char8 (pack)

import           External.ReservationRestService
import           UseCases.ReservationUseCase
import           UseCases.Config
import qualified Database.SQLite.Simple as SQL
import Polysemy.Trace (traceToIO)
import Polysemy.Input (runInputConst)


-- | creates the WAI Application that can be executed by Warp.run.
-- All Polysemy interpretations must be executed here.
createApp :: Config -> IO Application
createApp config = do
  return (serve reservationAPI $ hoistServer reservationAPI (interpretServer config) reservationServer)
  where
    interpretServer config sem  =  sem
      & runKVStoreAsSQLite
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
  let config = Config {maxCapacity = 20, port = 8080, dbPath = "kvs.db"} -- In real life this will be external configuration like commandline parameters or a configuration file.
  app  <- createApp config
  putStrLn $ "Starting server on port " ++ show (port config)
  Warp.run (port config) app
