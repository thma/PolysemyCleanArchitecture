module Integration.PersistenceSpec where

import           Test.Hspec

import qualified Data.Map as Map
import           Data.Time.Calendar
import Control.Monad.Cont (liftIO)

import Domain.ReservationBusinessLogic


main :: IO ()
main = hspec spec

day1 = fromGregorian 2020 1 29
reservation1 = Reservation day1 "Andrew M. Jones" "amjones@example.com" 4
reservation2 = Reservation day1 "Thomas Miller" "tm@example.com" 3
--table = Map.fromList [(day1, [reservation1, reservation2])] :: ReservationTable
totalCapacity = 20
restaurant = "MyMumMade"

spec :: Spec
spec = undefined
{--
spec =
  describe "persistence" $ do
    it "stores a Reservation Table" $ do
      let tab = return table :: IO ReservationTable
      storeTable tab restaurant `shouldReturn` ()
      return () :: IO ()
      
    it "loads a Reservation Table from disk" $ do
      let tab = return table :: IO ReservationTable
          restaurant = "My Dad Made"
      storeTable tab restaurant
      loadTable restaurant `shouldReturn` table
 
    it "loads an empty table if there is no existing time table for a given restaurant" $ do
      let restaurant = "My Sister Made"
      loadTable restaurant `shouldReturn` Map.fromList []

--}