module ExternalInterfacesSpec where

import qualified Data.ByteString.Lazy                     as LB
import           ExternalInterfaces.ApplicationAssembly   (createApp)
import           InterfaceAdapters.Config
import           Network.HTTP.Types.Header                (hContentType)
import           Network.HTTP.Types.Method                (methodDelete,
                                                           methodPost)
import           Test.Hspec
import           Test.Hspec.Wai
import Network.Wai.Test (SResponse)
import Data.ByteString.Internal

reservationData :: LB.ByteString
reservationData = "{\"email\":\"amjones@example.com\",\"quantity\":12,\"date\":\"2020-05-02\",\"name\":\"Amelia Jones\"}"

postJSON :: ByteString -> LB.ByteString -> WaiSession SResponse
postJSON path = request methodPost path [(hContentType, "application/json")]

deleteJSON :: ByteString -> LB.ByteString -> WaiSession SResponse
deleteJSON path = request methodDelete path [(hContentType, "application/json")]

main :: IO ()
main = hspec spec

config :: Config
config = Config {port = 8080, backend = SQLite, dbPath = "kvs-assembly.db", verbose = False}

spec :: Spec
spec =
  with (return $ createApp config) $
    describe "Rest Service" $ do

      it "responds with 20 for a first call to GET /seats/YYYY-MM-DD" $
        get "/seats/2020-05-02" `shouldRespondWith` "20"

      it "responds with 200 for a valid POST /reservations" $
        postJSON "/reservations" reservationData `shouldRespondWith` 200

      it "responds with 200 for a call GET /reservations " $
        get "/reservations" `shouldRespondWith` "{\"2020-05-02\":[{\"email\":\"amjones@example.com\",\"quantity\":12,\"date\":\"2020-05-02\",\"name\":\"Amelia Jones\"}]}"

      it "responds with 412 if a reservation can not be done on a given day" $
        (postJSON "/reservations" reservationData >> postJSON "/reservations" reservationData) `shouldRespondWith` 412

      it "responds with 20 for a first call to GET /seats/YYYY-MM-DD" $
        get "/seats/2020-05-02" `shouldRespondWith` "8"

      it "responds with 200 for a valid DELETE /reservations" $
        deleteJSON "/reservations" reservationData `shouldRespondWith` 200

