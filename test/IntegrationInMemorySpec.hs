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

import           Effects.KVS
import           Integration.ReservationIntegration

import Domain.ReservationBusinessLogic
import Polysemy.Input (Input, runInputConst)
import Integration.Config

main :: IO ()
main = hspec spec

-- | Takes a program with effects and handles each effect till it gets reduced to [Either ReservationError (ReservationMap, a)]. No IO !
runPure :: ReservationMap 
        -> (forall r. Members [ReservationTable, Error ReservationError, Trace, Input Config] r => Sem r a)  
        -> [Either ReservationError (ReservationMap, a)]
runPure kvsMap program = 
  program
     & runKvsPure kvsMap
     & runInputConst config
     & runError @ReservationError
     & ignoreTrace
     & runM
  where
    config = Config {maxCapacity = 20, port = 8080}

-- Helper functions for interpreting all effects in a pure way. That is no IO !
runTryReservation :: ReservationMap -> Reservation -> Maybe ReservationMap
runTryReservation kvsMap res = do
  case runPure kvsMap (tryReservation res) of
    [Right (m, ())] -> Just m
    [Left err]      -> Nothing

runFetch :: ReservationMap -> Day -> Maybe [Reservation]
runFetch kvsMap day = do
  case runPure kvsMap (fetch day) of
    [Right (_, maybe)] -> maybe
    [Left err]         -> error "fetch failed"
  
runListAll :: ReservationMap -> ReservationMap
runListAll kvsMap = do
  case runPure kvsMap (listAll) of
    [Right (_, m)] -> m
    [Left err]     -> error "listALl failed" 
  
-- setting up test fixtures
initReservations :: ReservationMap
initReservations = M.singleton day res

day = fromGregorian 2020 5 2
res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

spec :: Spec
spec =
  describe "Integration Layer" $ do
    it "fetches a list of reservations from the KV store" $ do
      (runFetch initReservations day) `shouldBe` (Just res)
      
    it "returns Nothing if there are no reservations for a given day" $ do
      let kvsMap = M.fromList []
      (runFetch kvsMap day) `shouldBe` Nothing
      
    it "can retrieve a map of all reservations" $ do
      let m = runListAll initReservations
      M.size m `shouldBe` 1
      
    it "can add a reservation if there are enough free seats" $ do
      let goodReservation = Reservation day "Gabriella. Miller" "gm@example.com" 4
      let m = runTryReservation initReservations goodReservation
          maybeMatch = case m of
            Just map -> runFetch map day
            Nothing  -> Nothing
      case maybeMatch of
        Nothing -> fail "no reservations found"
        Just reservations -> (goodReservation `elem` reservations `shouldBe` True)

    it "reports an erorr if a reservation is not possible" $ do
      let badReservation = Reservation day "Gabriella. Miller" "gm@example.com" 17
      (runTryReservation initReservations badReservation) `shouldBe` Nothing   
      