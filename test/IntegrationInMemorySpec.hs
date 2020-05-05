module IntegrationInMemorySpec where

import           Test.Hspec

import           Control.Exception
import           Control.Monad.Except
import           Data.Function                      ((&))
import           Data.IORef
import qualified Data.Map.Strict                    as M
import           Data.Time.Calendar
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Trace                     (Trace, traceToIO, ignoreTrace)

import           Integration.KVS
import           Integration.ReservationIntegration

import Domain.ReservationBusinessLogic

main :: IO ()
main = hspec spec

-- Helper functions for interpreting all effects in a testable way

-- | Takes a program with effects and handles each effect till it gets reduced to IO a.
runAllEffects :: IORef ReservationMap
              -> (forall r. Members [ReservationTable, Error ReservationError, Trace] r => Sem r a)
              -> IO a
runAllEffects kvsIORef program =
  program
    & runKvsOnMapState
    & runStateIORef @(ReservationMap) kvsIORef
    & runError @ReservationError
    & ignoreTrace
    & runM
    & handleErrors

runPure :: (forall r. Members [ReservationTable, Error ReservationError, Trace] r => Sem r a) -> [(Either ReservationError a)]
runPure program = 
  program
     & runKvsPure
     & runError @ReservationError
     & ignoreTrace
     & runM

-- errors are rethrown as Runtime errors, which can be verified by HSpec.
handleErrors :: IO (Either ReservationError a) -> IO a
handleErrors e = do
  either <- e
  case either of
    Right v                           -> return v
    Left (ReservationNotPossible msg) -> (error msg)

runTryReservation :: (IORef ReservationMap) -> Reservation -> IO ()
runTryReservation kvsIORef res = do
  runAllEffects kvsIORef (tryReservation res)

runFetch :: (IORef ReservationMap) -> Day -> IO (Maybe [Reservation])
runFetch kvsIORef day = do
  runAllEffects kvsIORef (fetch day)
  
runListAll :: (IORef ReservationMap) -> IO ReservationMap
runListAll kvsIORef = do
  runAllEffects kvsIORef (listAll)
  
runPureFetch :: Day -> [Either ReservationError (Maybe [Reservation])]
runPureFetch day = do
  runPure (fetch day)  

-- function for setting up test fixtures
initReservations :: ReservationMap
initReservations = M.singleton day res -- M.fromList [] --

day = fromGregorian 2020 5 2
res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

spec :: Spec
spec =
  describe "Integration Layer" $ do
    it "fetches a list of reservations from the KV store" $ do
      kvsIORef <- newIORef $ initReservations
      maybeMatch <- runFetch kvsIORef day
      maybeMatch `shouldBe` (Just res)
    it "returns Nothing if there are no reservations for a given day" $ do
      kvsIORef <- newIORef $ M.fromList []
      maybeMatch <- runFetch kvsIORef day
      maybeMatch `shouldBe` Nothing
    it "can retrieve a map of all reservations" $ do
      kvsIORef <- newIORef $ initReservations
      map <- runListAll kvsIORef
      M.size map `shouldBe` 1
    it "can add a reservation if there are enough free seats" $ do
      kvsIORef <- newIORef $ initReservations
      let goodReservation = Reservation day "Gabriella. Miller" "gm@example.com" 4
      runTryReservation kvsIORef goodReservation
      map <- runListAll kvsIORef
      maybeMatch <- runFetch kvsIORef day
      case maybeMatch of
        Nothing -> fail "no reservations found"
        Just reservations -> (goodReservation `elem` reservations `shouldBe` True)
    it "throws an erorr if a reservation is not possible" $ do
      kvsIORef <- newIORef $ initReservations
      let badReservation = Reservation day "Gabriella. Miller" "gm@example.com" 17
      runTryReservation kvsIORef badReservation `shouldThrow` (errorCall $ "we are fully booked on " ++ show day)   
      
    it "should fetch an empty list when using the pure KVS implementation" $ do
      let [res] = runPureFetch day
      res `shouldBe` Right Nothing -- :: Either ReservationError (Maybe [Reservation]))
      --res `shouldBe` Right Nothing