{-# LANGUAGE OverloadedStrings #-}

module InterfaceAdaptersRestServiceSpec where

import           Control.Monad.Except
import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Lazy            as LB
import           Data.ByteString.Lazy.Char8      (pack)
import           Data.Function                   ((&))
import           Data.IORef
import qualified Data.Map.Strict                 as M
import           Data.Time.Calendar
import           InterfaceAdapters.ReservationRestService
import           InterfaceAdapters.Config
import           InterfaceAdapters.KVSSqlite     (runKvsAsSQLite)
import           Network.HTTP.Types.Header       (hContentType)
import           Network.HTTP.Types.Method       (methodDelete, methodPost)
import qualified Network.Wai.Handler.Warp        as W
import           Network.Wai.Test                hiding (request)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input                  (runInputConst)
import           Polysemy.State                  hiding (get)
import           Polysemy.Trace                  (ignoreTrace, traceToIO)
import           Servant.Server
import           Test.Hspec
import           Test.Hspec.Wai
import           UseCases.ReservationUseCase

createApp :: Config -> IO Application
createApp config = return $ serve reservationAPI (liftServer config)

liftServer :: Config -> ServerT ReservationAPI Handler
liftServer config = hoistServer reservationAPI (interpretServer config) reservationServer
  where
    interpretServer config sem =
      sem
        & runKvsAsSQLite
        & runInputConst config
        & runError @ReservationError
        & ignoreTrace
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 {errBody = pack msg}
    handleErrors (Right value) = Right value

reservationData :: LB.ByteString
reservationData = "{\"email\":\"amjones@example.com\",\"quantity\":10,\"date\":\"2020-05-02\",\"name\":\"Amelia Jones\"}"

postJSON path = request methodPost path [(hContentType, "application/json")]
deleteJSON path = request methodDelete path [(hContentType, "application/json")]

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (createApp config) $
    describe "Rest Service" $ do
      it "responds with 200 for a call  GET    /seats " $
        get "/seats/2020-05-02" `shouldRespondWith` "20"
      it "responds with 200 for a valid POST   /reservations" $
        postJSON "/reservations" reservationData `shouldRespondWith` 200
      it "responds with 200 for a call  GET    /reservations " $
        get "/reservations" `shouldRespondWith` expected
      it "responds with 412 if a reservation can not be done on a given day" $
        (postJSON "/reservations" reservationData >> postJSON "/reservations" reservationData) `shouldRespondWith` 412
      it "responds with 200 for a valid DELETE /reservations" $
        (deleteJSON "/reservations" reservationData >> deleteJSON "/reservations" reservationData) `shouldRespondWith` 200
      it "responds with 200 for a call  GET    /seats " $
        get "/seats/2020-05-02" `shouldRespondWith` "20"
  where
    config = Config {port = 8080, dbPath = "kvs-ia-test.db", backend = SQLite, verbose = False}
    expected = "{\"2020-05-02\":[{\"email\":\"amjones@example.com\",\"quantity\":10,\"date\":\"2020-05-02\",\"name\":\"Amelia Jones\"}]}"