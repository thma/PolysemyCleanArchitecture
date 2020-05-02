{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module Rest.Tester where

import           Data.Function ((&))
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
--import           Polysemy.Resource
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
              -> IO a
runAllEffects kvsIORef program =
  program
    & runKvsOnMapState
    & runStateIORef @(ReservationMap) kvsIORef
--    & fromEither -- @(Either ReservationError)
    & runError @ReservationError
    & traceToIO
    & runM
    & liftToHandler


liftToHandler :: IO (Either ReservationError a) -> IO a
liftToHandler e = do
  either <- e
  handleErrors either

-- lliftToHandler = Handler . ExceptT . (fmap _handleErrors)

handleErrors :: Either ReservationError a -> IO a
--handleErrors (Left (ReservationNotPossible msg)) = return . id 
handleErrors (Right x) = return x
--handleErrors (Right value) = Right value

initReservations :: ReservationMap
initReservations = M.singleton day res
  where
    day = fromGregorian 2020 5 2
    res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

main :: IO ()
main = do
  putStrLn "testing the reservation API without a REST server"
  kvsIORef <- newIORef initReservations
  let day = fromGregorian 2020 5 2
  let reservation1 = Reservation day "Gabriella Jones" "gjones@example.com" 16

  runAllEffects kvsIORef (tryReservation reservation1)
  --putStrLn "done.."


