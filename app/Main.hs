module Main where

import           Data.Function ((&))
import           InterfacesAdapters.KVSFileServer (runKvsAsFileServer)
import qualified Network.Wai.Handler.Warp as W
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
import Polysemy.Trace (traceToIO)
import Polysemy.Input (runInputConst)

initReservations :: ReservationMap
initReservations = M.singleton day res
  where
    day = fromGregorian 2020 5 2
    res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

createApp :: Config -> IO Application
createApp config = do
  return (serve reservationAPI $ hoistServer reservationAPI (interpretServer config) reservationServer)
  where
    interpretServer config sem  =  sem
      & runKvsAsFileServer
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
  let config = Config {maxCapacity = 20, port = 8080}
  let p = port config
  app <- createApp config
  putStrLn $ "Starting server on port " ++ show p
  W.run p app
