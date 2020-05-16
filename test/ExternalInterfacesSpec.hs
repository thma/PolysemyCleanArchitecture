module ExternalInterfacesSpec where

import           Control.Monad.Except
import           Data.ByteString                          (ByteString)
import qualified Data.ByteString.Lazy                     as LB
import           Data.ByteString.Lazy.Char8               (pack)
import           Data.Function                            ((&))
import           Data.IORef
import qualified Data.Map.Strict                          as M
import           Data.Time.Calendar
import           ExternalInterfaces.ApplicationAssembly   (createApp)
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSInMemory
import           InterfaceAdapters.ReservationRestService
import           Network.HTTP.Types.Header                (hContentType)
import           Network.HTTP.Types.Method                (methodDelete,
                                                           methodPost)
import qualified Network.Wai.Handler.Warp                 as W
import           Network.Wai.Test                         hiding (request)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                           (runInputConst)
import           Polysemy.State                           hiding (get)
import           Polysemy.Trace                           (ignoreTrace,
                                                           traceToIO)
import           Servant.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           UseCases.ReservationUseCase

reservationData :: LB.ByteString
reservationData = "{\"email\":\"amjones@example.com\",\"quantity\":12,\"date\":\"2020-05-02\",\"name\":\"Amelia Jones\"}"

postJSON path = request methodPost path [(hContentType, "application/json")]
deleteJSON path = request methodDelete path [(hContentType, "application/json")]

main :: IO ()
main = hspec spec


loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = SQLite, dbPath = "kvs-assembly.db"}

spec :: Spec
spec =
  with (loadConfig >>= createApp) $
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

