module BusinessLogic.ReservationBusinessLogicSpec where

import           Test.Hspec
import qualified Data.Map as Map
import           Data.Time.Calendar

import           BL.ReservationBusinessLogic

main :: IO ()
main = hspec spec

day = fromGregorian 2020 1 29
reservation1 = Reservation day "Andrew M. Jones" "amjones@example.com" 4
reservation2 = Reservation day "Thomas Miller" "tm@example.com" 3
table = Map.fromList [(day, [reservation1, reservation2])]
totalCapacity = 20

spec :: Spec
spec =
  describe "reservationLogic" $ do
    it "computes the used capacity for an empty list of reservations" $
      usedCapacity [] `shouldBe` 0
      
    it "computes the used capacity for a list of reservations" $
      usedCapacity [reservation1, reservation2] `shouldBe` 7
      
    it "computes the available capacity for a given day" $
      availableCapacity table day totalCapacity `shouldBe` 13
      
    it "can add a reservation to a reservation table" $ 
      let updatedTable = addReservation table (Reservation day "name" "mail@mail.com" 8)
       in availableCapacity updatedTable day totalCapacity `shouldBe` 5
       
    it "returns Nothing if reservation is not possible" $
      tryReservation table reservation1 10 `shouldBe` Nothing
      
    it "returns updatedTable if reservation is possible" $
      tryReservation table reservation2 10 `shouldBe` 
        Just (Map.fromList [(read "2020-01-29",
          [Reservation {date = read "2020-01-29", name = "Thomas Miller", email = "tm@example.com", quantity = 3},
           Reservation {date = read "2020-01-29", name = "Andrew M. Jones", email = "amjones@example.com", quantity = 4},
           Reservation {date = read "2020-01-29", name = "Thomas Miller", email = "tm@example.com", quantity = 3}])])

