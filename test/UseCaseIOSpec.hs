module UseCaseIOSpec where

import           Test.Hspec

import           Data.Function                    ((&))
import           Data.List                        (isSuffixOf)
import qualified Data.Map.Strict                  as M
import           Data.Time.Calendar
import           Domain.ReservationDomain
import           InterfaceAdapters.KVSFileServer
import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace                   (Trace, ignoreTrace)
import           System.Directory                 (listDirectory, removeFile)
import           UseCases.ReservationUseCase
import Control.Monad.Cont (liftIO)



main :: IO ()
main = hspec spec

-- | Takes a program with effects and handles each effect till it gets reduced to IO a.
runAllEffects :: Sem '[Persistence, Error ReservationError, Trace, Embed IO] a -> IO a
runAllEffects program =
  program
    & runKvsAsFileServer
    & runError @ReservationError
    & ignoreTrace
    & runM
    & handleErrors

-- errors are rethrown as Runtime errors, which can be verified by HSpec.
handleErrors :: IO (Either ReservationError a) -> IO a
handleErrors e = do
  eitherErr <- e
  case eitherErr of
    Right v                           -> return v
    Left (ReservationNotPossible _msg) ->
      case eitherErr of
        Left (ReservationNotPossible msg) -> error msg

-- Helper functions for interpreting all effects in IO
runTryReservation :: Reservation -> IO ()
runTryReservation r = do
  runAllEffects (tryReservation r)

runFetch :: Day -> IO [Reservation]
runFetch d = do
  runAllEffects (fetch d)

runListAll :: IO ReservationMap
runListAll = do
  runAllEffects listAll

runCancel :: Reservation -> IO ()
runCancel r = do
  runAllEffects (cancel r)


-- | helper function to clean the test data files
deleteAllFiles :: IO [()]
deleteAllFiles = do
  allFiles <- liftIO $ listDirectory dataDir
  let filteredFiles = filter (isSuffixOf ".json") allFiles
  mapM (liftIO . removeFile . (dataDir ++)) filteredFiles


day :: Day
day = fromGregorian 2020 5 2

res :: [Reservation]
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
