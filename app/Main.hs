{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Main where

import           Data.IORef
import           Data.Function ((&))
import           Integration.KVS
import qualified Network.Wai.Handler.Warp as W
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Servant.Server

import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.Time.Calendar
import           Data.ByteString.Lazy.Char8 (pack)

import           Rest.ReservationService
import           Integration.ReservationIntegration

initReservations :: ReservationMap
initReservations = M.singleton day res
  where
    day = fromGregorian 2020 5 2
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
