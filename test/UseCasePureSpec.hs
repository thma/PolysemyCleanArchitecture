module UseCasePureSpec where

import           Control.Exception
import           Control.Monad.Except
import           Data.Function                  ((&))
import           Data.IORef
import qualified Data.Map.Strict                as M
import           Data.Time.Calendar
import           Domain.ReservationDomain
import           InterfaceAdapters.KVSInMemory
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                 (Input, runInputConst)
import           Polysemy.State
import           Polysemy.Trace                 (Trace, ignoreTrace, traceToIO)
import           Test.Hspec
import qualified UseCases.ReservationUseCase    as UC

main :: IO ()
main = hspec spec

-- | Takes a program with effects and handles each effect till it gets reduced to Either ReservationError (ReservationMap‚ a). No IO !
runPure :: ReservationMap
        -> (forall r. Members [UC.Persistence, Error UC.ReservationError, Trace] r => Sem r a)
        -> Either UC.ReservationError (ReservationMap, a)
runPure kvsMap program =
  program
     & runKvsPure kvsMap              -- run the key-value store on a simple ReservationMap
     & runError @UC.ReservationError  -- run error handling to produce an Either UC.ReservationError (ReservationMap, a)
     & ignoreTrace                    -- run Trace by simply ignoring all messages 
     & run                            -- run a 'Sem' containing no effects as a pure value

-- Helper functions for interpreting all effects in a pure way. That is no IO !

runAvailableSeats :: ReservationMap -> Day -> Natural
runAvailableSeats kvsMap day = do
  case runPure kvsMap (UC.availableSeats day) of
    Right (_, numSeats) -> numSeats
    Left err            -> error "availableSeats failed"

runTryReservation :: ReservationMap -> Reservation -> Maybe ReservationMap
runTryReservation kvsMap res = do
  case runPure kvsMap (UC.tryReservation res) of
    Right (m, ()) -> Just m
    Left err      -> Nothing

runFetch :: ReservationMap -> Day -> [Reservation]
runFetch kvsMap day = do
  case runPure kvsMap (UC.fetch day) of
    Right (_, reservations) -> reservations
    Left err                -> error "fetch failed"

runListAll :: ReservationMap -> ReservationMap
runListAll kvsMap = do
  case runPure kvsMap (UC.listAll) of
    Right (_, m) -> m
    Left err     -> error "listALl failed"

runCancel :: ReservationMap -> Reservation -> Maybe ReservationMap
runCancel kvsMap res = do
  case runPure kvsMap (UC.cancel res) of
    Right (m, ()) -> Just m
    Left err      -> Nothing

-- setting up test fixtures
initReservations :: ReservationMap
initReservations = M.singleton day res

day = read "2020-05-02"
res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

spec :: Spec
spec =
  describe "Reservation Use Case (only pure code)" $ do
  
    it "computes the number of available seats for a given day" $ do
      (runAvailableSeats initReservations day) `shouldBe` 16
  
    it "fetches a list of reservations from the KV store" $ do
      (runFetch initReservations day) `shouldBe` res

    it "returns Nothing if there are no reservations for a given day" $ do
      let kvsMap = M.fromList []
      (runFetch kvsMap day) `shouldBe` []

    it "can retrieve a map of all reservations" $ do
      let m = runListAll initReservations
      M.size m `shouldBe` 1

    it "can add a reservation if there are enough free seats" $ do
      let goodReservation = Reservation day "Gabriella. Miller" "gm@example.com" 4
      let m = runTryReservation initReservations goodReservation
          reservations = case m of
            Just map -> runFetch map day
            Nothing  -> []
      goodReservation `elem` reservations `shouldBe` True

    it "reports an error if a reservation is not possible" $ do
      let badReservation = Reservation day "Gabriella. Miller" "gm@example.com" 17
      (runTryReservation initReservations badReservation) `shouldBe` Nothing

    it "cancels a reservation by deleting it from the KV store" $ do
      let res1 = res !! 0
          res2 = Reservation day "Gabriella. Miller" "gm@example.com" 5
          kvsMap = M.fromList [(day, [res1, res2])]
          m = runCancel kvsMap res1
          reservations = case m of
            Just map -> runFetch map day
            Nothing  -> []
      reservations `shouldBe` [res2]
