{-# LANGUAGE OverloadedStrings #-}
module RestService.ServiceSpec where

import           Rest.ReservationService       (app)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (return app) $ 
  describe "GET /reservations" $
  it "responds with 200" $
    get "/reservations" `shouldRespondWith` 200
