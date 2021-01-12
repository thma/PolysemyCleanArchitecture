module DomainSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Data.Time.Calendar

import           Domain.ReservationDomain
import           GHC.Natural (Natural (..), naturalFromInteger)

main :: IO ()
main = hspec spec

instance Arbitrary Natural where
  arbitrary = do
    NonNegative nonNegative <- arbitrary
    return $ naturalFromInteger nonNegative

instance Arbitrary Reservation where
  arbitrary = do
    natural <- arbitrary
    return $ Reservation day "Jupp0" "jupp@jupp.com" natural


day :: Day
day = fromGregorian 2020 2 29

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
      property $ \reservations -> usedCapacity reservations `shouldBe` sum (map quantity reservations)
      
    it "computes the available seats for a list of reservations" $
      property $ 
        \reservations maxCapacity ->
          availableSeats maxCapacity reservations `shouldBe` 
          if maxCapacity >= (usedCapacity reservations) 
            then maxCapacity - (usedCapacity reservations) 
            else 0

    it "can check if a reservation is possible on a given day" $ 
      property $ \res reservationsOnDay maxCapacity -> 
        isReservationPossible res reservationsOnDay maxCapacity `shouldBe`
        availableSeats maxCapacity reservationsOnDay >= quantity res

    it "can check if a reservation is possible on a day with no bookings" $ 
      property $ \res maxCapacity -> isReservationPossible res [] maxCapacity `shouldBe` quantity res <= maxCapacity

    it "will accept all reservations up to available seats on a day with no bookings" $
      property $ \reservation available -> isReservationPossible reservation [] available `shouldBe` (quantity reservation) <= available 

    it "detects if a reservation is not possible on a given day" $ 
      isReservationPossible (Reservation day "name" "mail@mail.com" 15) reservations totalCapacity `shouldBe` False

    it "can add a reservation to a list of reservations" $ do
      property $ \quantity reservations -> 
        let res = Reservation day "x" "mail@mail.com" quantity
            added = addReservation res reservations 
        in length added == 1 + length reservations &&
              not (res `elem` reservations)        &&
              res `elem` added                     &&
              usedCapacity added == (usedCapacity reservations) + quantity

    it "can cancel a reservation" $ do
      cancelReservation res1 reservations `shouldBe` [res2]
      cancelReservation res1 [] `shouldBe` []
      cancelReservation res2 reservations `shouldBe` [res1]
      cancelReservation res2 (cancelReservation res1 reservations) `shouldBe` []
