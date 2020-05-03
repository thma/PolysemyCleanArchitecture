{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Rest.Tester where

import           Data.Function ((&))
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import Polysemy.Trace (Trace, traceToIO)
import qualified Data.Map.Strict as M
import           Data.IORef
import           Data.Time.Calendar

import Integration.ReservationIntegration
import Integration.KVS

import Control.Monad.Except
import Control.Exception

-- | Takes a program with effects and handles each effect till it gets
-- reduced to IO a.
--
-- The comments on the rightside of each line below indicates the list
-- of effects that still need to be handled at that point.
--

runAllEffects :: IORef ReservationMap
              -> (forall r. Members [ReservationTable, Error ReservationError, Trace] r => Sem r a)
              -> IO (Either ReservationError a)
runAllEffects kvsIORef program =
  program
    & runKvsOnMapState
    & runStateIORef @(ReservationMap) kvsIORef
    & runError @ReservationError
    & traceToIO
    & runM

initReservations :: ReservationMap
initReservations = M.singleton day res -- M.fromList [] --
  where
    day = fromGregorian 2020 5 2
    res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

--runTryReservation :: Reservation -> (IORef ReservationMap) -> IO Bool
--runTryReservation res kvsIORef = do
--  runAllEffects kvsIORef (tryReservation res)
--  return True
  
main :: IO (Either ReservationError ())
main = do
  putStrLn "testing the reservation API without a REST server"
  kvsIORef <- newIORef initReservations
  let day = fromGregorian 2020 5 2
  let reservation1 = Reservation day "Gabriella Jones" "gjones@example.com" 4

  runAllEffects kvsIORef 
    (do tryReservation reservation1 
        listAll 
        tryReservation reservation1 
        tryReservation reservation1 
        tryReservation reservation1 
        listAll
        tryReservation reservation1
    )
  
  --return $ Right ()


