{-# LANGUAGE OverloadedStrings #-}
module RestService.ServiceSpec where

--import           Main       (createApp)
import           Test.Hspec
import           Test.Hspec.Wai
--import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  return ()
{--
spec =
  with (return createApp) $ 
  describe "GET /reservations" $
  it "responds with 200" $
    get "/reservations" `shouldRespondWith` 200
--}