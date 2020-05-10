{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module ExternalSpec where

import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8         (pack)
import           Data.Function                      ((&))
import           Data.IORef
import qualified Data.Map.Strict                    as M
import           Data.Time.Calendar
import           InterfacesAdapters.KVSInMemory
import           UseCases.ReservationUseCase
import           UseCases.Config
import qualified Network.Wai.Handler.Warp           as W
import           Polysemy
import           Polysemy.Error
import           Polysemy.State                     hiding (get)
import           Polysemy.Trace                     (traceToIO, ignoreTrace)
import           External.ReservationRestService
import           Servant.Server

import           Test.Hspec
import           Test.Hspec.Wai
import qualified Data.ByteString.Lazy as LB
import           Data.ByteString (ByteString)
import           Network.Wai.Test hiding (request)
import Network.HTTP.Types.Method (methodPost, methodDelete)
import Network.HTTP.Types.Header (hContentType)
import Polysemy.Input (runInputConst)


initReservations :: ReservationMap
initReservations = M.singleton day res
  where
    day = fromGregorian 2020 5 2
    res = [Reservation day "Andrew M. Jones" "amjones@example.com" 4]

createApp :: IO Application
createApp = do
  kvsIORef <- newIORef initReservations
  return (serve reservationAPI $ hoistServer reservationAPI (\sem -> interpretServer sem kvsIORef) reservationServer)
  where
    interpretServer sem kvsIORef =
      sem
        & runKvsOnMapState
        & runStateIORef @(ReservationMap) kvsIORef
        & runInputConst config
        & runError @ReservationError
        & ignoreTrace
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 {errBody = pack msg}
    handleErrors (Right value) = Right value
    config = Config {maxCapacity = 20, port = 8080, dbPath = "kvs.db"}


reservationData :: LB.ByteString
reservationData = "{\"email\":\"amjones@example.com\",\"quantity\":10,\"date\":\"2020-05-02\",\"name\":\"Amelia Jones\"}"

postJSON path = request methodPost path [(hContentType, "application/json")]
deleteJSON path = request methodDelete path [(hContentType, "application/json")]

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (createApp) $
    describe "Rest Service" $ do
      it "responds with 200 for a call GET /reservations " $
        get "/reservations" `shouldRespondWith` "{\"2020-05-02\":[{\"email\":\"amjones@example.com\",\"quantity\":4,\"date\":\"2020-05-02\",\"name\":\"Andrew M. Jones\"}]}"
      it "reponds with 200 for a valid POST /reservations" $
        postJSON "/reservations" reservationData `shouldRespondWith` 200
      it "reponds with 412 if a reservation can not be done on a given day" $
        (postJSON "/reservations" reservationData >> postJSON "/reservations" reservationData) `shouldRespondWith` 412
      it "reponds with 200 for a valid DELETE /reservations" $
        deleteJSON "/reservations" reservationData `shouldRespondWith` 200
        
     
