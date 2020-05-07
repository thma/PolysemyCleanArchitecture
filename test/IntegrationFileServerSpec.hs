module IntegrationFileServerSpec where

import           Test.Hspec

import           Control.Exception
import           Control.Monad.Except
import           Data.Function                      ((&))
import qualified Data.Map.Strict                    as M
import           Data.Time.Calendar
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Trace                     (Trace, traceToIO, ignoreTrace)
import           System.Directory  (doesFileExist, listDirectory, removeFile)


import           Effects.KVS
import           Integration.ReservationIntegration
import           Integration.Config

import Domain.ReservationBusinessLogic
import           Effects.KVSFileServer
import Polysemy.Input (Input, runInputConst)

main :: IO ()
main = hspec spec

-- -- | Takes a program with effects and handles each effect till it gets reduced to IO a.
runAllEffects :: (forall r. Members [ReservationTable, Error ReservationError, Trace, Input Config] r => Sem r a) -> IO a
runAllEffects program =
  program
    & runKvsAsFileServer
    & runInputConst config
    & runError @ReservationError
    & ignoreTrace
    & runM
    & handleErrors
  where config = Config {maxCapacity = 20, port = 8080}

-- errors are rethrown as Runtime errors, which can be verified by HSpec.
handleErrors :: IO (Either ReservationError a) -> IO a
handleErrors e = do
  either <- e
  case either of
    Right v                           -> return v
    Left (ReservationNotPossible msg) -> (error msg)

-- Helper functions for interpreting all effects in IO
runTryReservation :: Reservation -> IO ()
runTryReservation res = do
  runAllEffects (tryReservation res)

runFetch :: Day -> IO (Maybe [Reservation])
runFetch day = do
  runAllEffects (fetch day)
  
runListAll :: IO ReservationMap
runListAll = do
  runAllEffects (listAll)
  
deleteAllFiles :: IO [()]
deleteAllFiles = do
  allFiles <- listDirectory dataDir
  mapM removeFile (map (\f -> dataDir ++ f) allFiles)
  

day = fromGregorian 2020 5 2
res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

spec :: Spec
spec =
  describe "Integration Layer" $ do
    it "needs a file cleaning for repeatable tests in the file system..." $ do
      result <- deleteAllFiles
      result  `shouldBe` [()]
    it "returns Nothing if there are no reservations for a given day" $ do
      maybeMatch <- runFetch day
      maybeMatch `shouldBe` Nothing
 
    it "can add a reservation if there are enough free seats" $ do
      let goodReservation = head res
      runTryReservation goodReservation
      map <- runListAll 
      maybeMatch <- runFetch day
      case maybeMatch of
        Nothing -> fail "no reservations found"
        Just reservations -> (goodReservation `elem` reservations `shouldBe` True)
      
    it "fetches a list of reservations from the KV store" $ do
      maybeMatch <- runFetch day
      maybeMatch `shouldBe` (Just res)

    it "can retrieve a map of all reservations" $ do
      map <- runListAll 
      M.size map `shouldBe` 1

    it "throws an erorr if a reservation is not possible" $ do
      let badReservation = Reservation day "Gabriella. Miller" "gm@example.com" 17
      runTryReservation badReservation `shouldThrow` (errorCall $ "we are fully booked on " ++ show day)   
      