{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module Rest.ServiceSpec where

import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8         (pack)
import           Data.Function                      ((&))
import           Data.IORef
import qualified Data.Map.Strict                    as M
import           Data.Time.Calendar
import           Integration.KVS
import           Integration.ReservationIntegration
import qualified Network.Wai.Handler.Warp           as W
import           Polysemy
import           Polysemy.Error
import           Polysemy.State                     hiding (get)
import           Polysemy.Trace                     (traceToIO)
import           Rest.ReservationService
import           Servant.Server

import           Test.Hspec
import           Test.Hspec.Wai


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
        & runError @ReservationError
        & traceToIO
        & runM
        & liftToHandler
    liftToHandler = Handler . ExceptT . (fmap handleErrors)
    handleErrors (Left (ReservationNotPossible msg)) = Left err409 {errBody = pack msg}
    handleErrors (Right value) = Right value

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (createApp) $ 
    describe "GET /reservations" $ 
      it "responds with 200" $ 
        get "/reservations" `shouldRespondWith` "{\"2020-05-02\":[{\"email\":\"amjones@example.com\",\"quantity\":4,\"date\":\"2020-05-02\",\"name\":\"Andrew M. Jones\"}]}"
