module DomainSpec where

import           Test.Hspec
import           Data.Time.Calendar

import           Domain.ReservationDomain
import           GHC.Natural (Natural)

main :: IO ()
main = hspec spec

day :: Day
day = fromGregorian 2020 1 29
res1 :: Reservation
res1 = Reservation day "Andrew M. Jones" "amjones@example.com" 4
res2 :: Reservation
res2 = Reservation day "Thomas Miller" "tm@example.com" 3
reservations :: [Reservation]
reservations = [res1, res2]

totalCapacity :: Natural
totalCapacity = 20

spec :: Spec
spec =
  describe "Domain Logic" $ do
    it "computes the used capacity for an empty list of reservations" $
      usedCapacity [] `shouldBe` 0

    it "computes the used capacity for a list of reservations" $
      usedCapacity [res1, res2] `shouldBe` 7
      
    it "computes the available seats for a list of reservations" $
      availableSeats totalCapacity [res1, res2] `shouldBe` 13

    it "can check if a reservation is possible on a given day" $ 
      isReservationPossible (Reservation day "name" "mail@mail.com" 8) reservations  totalCapacity `shouldBe` True

    it "can check if a reservation is possible on a day with no bookings" $ 
      isReservationPossible (Reservation day "name" "mail@mail.com" 8) []    totalCapacity `shouldBe` True

    it "detects if a reservation is not possible on a given day" $ 
      isReservationPossible (Reservation day "name" "mail@mail.com" 15) reservations totalCapacity `shouldBe` False

    it "can add a reservation to a list of reservations" $ do
      addReservation res1 [] `shouldBe` [res1]
      addReservation res1 reservations `shouldBe` res1:reservations

    it "can cancel a reservation" $ do
      cancelReservation res1 reservations `shouldBe` [res2]
      cancelReservation res1 [] `shouldBe` []
      cancelReservation res2 reservations `shouldBe` [res1]
      cancelReservation res2 (cancelReservation res1 reservations) `shouldBe` []
