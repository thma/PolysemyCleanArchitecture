module UseCaseIOSpec where

import           Test.Hspec

import           Control.Exception
import           Control.Monad.Except
import           Data.Function                    ((&))
import           Data.List                        (isSuffixOf)
import qualified Data.Map.Strict                  as M
import           Data.Time.Calendar
import           Domain.ReservationDomain
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSFileServer
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                   (Input, runInputConst)
import           Polysemy.State
import           Polysemy.Trace                   (Trace, ignoreTrace,
                                                   traceToIO)
import           System.Directory                 (doesFileExist, listDirectory,
                                                   removeFile)
import           UseCases.ReservationUseCase
import           Data.Aeson.Types (ToJSON, FromJSON)



main :: IO ()
main = hspec spec

-- | Takes a program with effects and handles each effect till it gets reduced to IO a.
runAllEffects :: (forall r. Members [Persistence, Error ReservationError, Trace, Input Config] r => Sem r a) -> IO a
runAllEffects program =
  program
    & runKvsAsFileServer
    & runInputConst config
    & runError @ReservationError
    & ignoreTrace
    & runM
    & handleErrors
  where config = Config {port = 8080, dbPath = "kvs.db", backend = FileServer}

-- | the FileServer implementation of KVS works with JSON serialization, thus Reservation must instantiate ToJSON and FromJSON
instance ToJSON Reservation
instance FromJSON Reservation

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

runFetch :: Day -> IO [Reservation]
runFetch day = do
  runAllEffects (fetch day)

runListAll :: IO ReservationMap
runListAll = do
  runAllEffects (listAll)

runCancel :: Reservation -> IO ()
runCancel r = do
  runAllEffects (cancel r)


-- | helper function to clean the test data files
deleteAllFiles :: IO [()]
deleteAllFiles = do
  allFiles <- listDirectory dataDir
  let filteredFiles = filter (isSuffixOf ".json") allFiles
  mapM removeFile (map (\f -> dataDir ++ f) filteredFiles)


day = fromGregorian 2020 5 2
res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

spec :: Spec
spec = do
  return deleteAllFiles
  describe "Reservation Use Case (with file IO)" $ do
    it "needs a file cleaning for repeatable tests in the file system..." $ do
      result <- deleteAllFiles
      map <- runListAll
      M.size map `shouldBe` 0

    it "returns Nothing if there are no reservations for a given day" $ do
      result <- runFetch day
      result `shouldBe` []

    it "can add a reservation if there are enough free seats" $ do
      let goodReservation = head res
      runTryReservation goodReservation
      map <- runListAll
      M.size map `shouldBe` 1
      reservations <- runFetch day
      goodReservation `elem` reservations `shouldBe` True

    it "fetches a list of reservations from the KV store" $ do
      result <- runFetch day
      result `shouldBe` res

    it "can retrieve a map of all reservations" $ do
      map <- runListAll
      M.size map `shouldBe` 1

    it "throws an error if a reservation is not possible" $ do
      let badReservation = Reservation day "Gabriella. Miller" "gm@example.com" 17
      runTryReservation badReservation `shouldThrow` (errorCall $ "Sorry, only 16 seats left on " ++ show day)

    it "can cancel a reservation" $ do
      let res1 = head res
      runTryReservation res1
      reservations <- runFetch day
      length reservations `shouldBe` 2

      runCancel res1

      reservations' <- runFetch day
      length reservations' `shouldBe` 1
