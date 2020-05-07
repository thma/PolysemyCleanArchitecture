module Main where

import           Data.Function ((&))
import           Effects.KVS
import           Effects.KVSFileServer (runKvsAsFileServer)
import qualified Network.Wai.Handler.Warp as W
import           Polysemy
import           Polysemy.Error
import           Servant.Server

import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.Time.Calendar
import           Data.ByteString.Lazy.Char8 (pack)

import           Rest.ReservationService
import           Integration.ReservationIntegration
import Polysemy.Trace (traceToIO)

initReservations :: ReservationMap
initReservations = M.singleton day res
  where
    day = fromGregorian 2020 5 2
    res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

createApp :: IO Application
createApp = do
  return (serve reservationAPI $ hoistServer reservationAPI interpretServer reservationServer)
  where
    interpretServer sem =  sem
      & runKvsAsFileServer
      & runError @ReservationError
      & traceToIO
      & runM
      & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value

main :: IO ()
main = do
  let port = 8080
  app <- createApp
  putStrLn $ "Starting server on port " ++ show port
  W.run port app
