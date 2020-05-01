{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import           Data.IORef
import           Data.Function ((&))
import           BL.KVS
import qualified Network.Wai.Handler.Warp as W
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Servant.Server
import           BL.ReservationBusinessLogic
import           Rest.ReservationService
import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.Time.Calendar
import           Data.ByteString.Lazy.Char8 (pack)

initReservations :: ReservationMap
initReservations = M.singleton day res
  where
    day = fromGregorian 2020 1 29
    res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

createApp :: IO Application
createApp = do
  kvsIORef <- newIORef initReservations
  return (serve reservationAPI $ hoistServer reservationAPI (\sem -> interpretServer sem kvsIORef) reservationServer)
  where
    interpretServer sem kvsIORef =  sem
                & runKvsOnMapState
                & runStateIORef @(ReservationMap) kvsIORef
                & runError @ReservationError
                & runM
                & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err409 { errBody = pack msg}
    handleErrors (Right value) = Right value

main :: IO ()
main = do
  let port = 8080
  app <- createApp
  putStrLn $ "Starting server on port " ++ show port
  W.run port app
