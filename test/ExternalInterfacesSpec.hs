module ExternalInterfacesSpec where

import qualified Data.ByteString.Lazy                     as LB
import           ExternalInterfaces.ApplicationAssembly   (createApp, loadConfig)
import           InterfaceAdapters.Config
import           Network.HTTP.Types.Header                (hContentType)
import           Network.HTTP.Types.Method                (methodDelete,
                                                           methodPost)
import           Test.Hspec
import           Test.Hspec.Wai
import           Domain.ReservationDomain
import qualified Data.Map.Strict as M
import           Data.Time (fromGregorian)
import           Data.Aeson (encode)
import           Test.Hspec.Wai.Matcher (bodyEquals)

reservation :: Reservation
reservation = Reservation (fromGregorian 2020 5 2) "Amelia Jones" "amjones@example.com" 12

reservationMap :: ReservationMap
reservationMap = M.fromList [(fromGregorian 2020 5 2, [reservation])]

reservationData :: LB.ByteString
reservationData = encode reservation


postJSON path = request methodPost path [(hContentType, "application/json")]

deleteJSON path = request methodDelete path [(hContentType, "application/json")]

main :: IO ()
main = hspec spec

config :: Config
config = Config {port = 8080, backend = SQLite, dbPath = "kvs-assembly.db", verbose = False}

spec :: Spec
spec = do
  it "can load the app config" $ do
    conf <- loadConfig 
    port    conf `shouldBe` 8080
    backend conf `shouldBe` SQLite 
    dbPath  conf `shouldBe` "kvs.db" 
    verbose conf `shouldBe` True

  with (return $ createApp (config {verbose = False, backend = FileServer})) $ 
    describe "Service with disabled tracing" $ do
      it "responds with 20 for a first call to GET /seats/YYYY-MM-DD" $
        get "/seats/2020-05-03" `shouldRespondWith` "20"

  with (return $ createApp config) $
    describe "Rest Service" $ do

      it "responds with 20 for a first call to GET /seats/YYYY-MM-DD" $
        get "/seats/2020-05-02" `shouldRespondWith` "20"

      it "responds with 200 for a valid POST /reservations" $
        postJSON "/reservations" reservationData `shouldRespondWith` 200

      it "responds with 200 for a call GET /reservations " $
        get "/reservations" `shouldRespondWith` expected

      it "responds with 412 if a reservation can not be done on a given day" $
        (postJSON "/reservations" reservationData >> postJSON "/reservations" reservationData) `shouldRespondWith` 412

      it "responds to call to GET /seats/YYYY-MM-DD with correct number when seats are already booked" $
        get "/seats/2020-05-02" `shouldRespondWith` "8"

      it "responds with 200 for a valid DELETE /reservations" $
        deleteJSON "/reservations" reservationData `shouldRespondWith` 200

      where
        expected :: ResponseMatcher
        expected = ResponseMatcher {matchBody = bodyEquals (encode reservationMap), matchStatus = 200, matchHeaders = []}


