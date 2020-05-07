module DomainSpec where

import           Test.Hspec
import qualified Data.Map as Map
import           Data.Time.Calendar

import           Domain.ReservationBusinessLogic

main :: IO ()
main = hspec spec

day = fromGregorian 2020 1 29
reservation1 = Reservation day "Andrew M. Jones" "amjones@example.com" 4
reservation2 = Reservation day "Thomas Miller" "tm@example.com" 3
list = [reservation1, reservation2]

--totalCapacity = 20

spec :: Spec
spec =
  describe "Domain Logic" $ do
    it "computes the used capacity for an empty list of reservations" $
      usedCapacity [] `shouldBe` 0

    it "computes the used capacity for a list of reservations" $
      usedCapacity [reservation1, reservation2] `shouldBe` 7

    it "computes the available capacity for a given day" $
      availableCapacity list 20 `shouldBe` 13
      
    it "can check if a reservation is possible on a given day" $ 
      isReservationPossible (Reservation day "name" "mail@mail.com" 8) list 20 `shouldBe` True

    it "can check if a reservation is possible on a day with no bookings" $ 
      isReservationPossible (Reservation day "name" "mail@mail.com" 8) [] 20 `shouldBe` True

    it "detects if a reservation is not possible on a given day" $ 
      isReservationPossible (Reservation day "name" "mail@mail.com" 15) list 20 `shouldBe` False
      
    it "can add a reservation to a list of reservations" $ do
      addReservation reservation1 [] `shouldBe` [reservation1] 
      addReservation reservation1 list `shouldBe` reservation1:list


