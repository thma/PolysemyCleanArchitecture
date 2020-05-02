{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Domain.ReservationBusinessLogic 
( Reservation (..)
, availableCapacity
, usedCapacity
, isReservationPossible
)

where

import           Data.Aeson      --(FromJSON, ToJSON)
import           GHC.Generics
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Data.Map.Strict as M

{--
This module implements the business logic for seat reservations in a very small boutique restaurant.
The restaurant has only one table with 20 tables.
Each day the restaurants accepts only 20 seat reservations. (There is no limited time-slot for each guest.)
--}

-- | a data type representing a reservation
data Reservation = Reservation {
     date     :: Day      -- ^ the date of the reservation
   , name     :: String   -- ^ the name of the guest placing the reservation
   , email    :: String   -- ^ the email address of the guest
   , quantity :: Int      -- ^ how many seats are requested
} deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | the total number of seats in the restaurant
maxCapacity :: Int
maxCapacity = 20   

-- | computes the number of available seats for the 
availableCapacity :: Maybe [Reservation] -> Int
availableCapacity Nothing     = maxCapacity
availableCapacity (Just res)  = maxCapacity - usedCapacity res

-- | computes the number of reserved seats for a list of reservations
usedCapacity :: [Reservation] -> Int
usedCapacity [] = 0
usedCapacity (Reservation _ _ _ quantity : rest) = quantity + usedCapacity rest

-- | check whether it is possible to add a reservation to the table.
-- | Return True if successful, else return False
isReservationPossible :: Reservation -> Maybe [Reservation] -> Bool
isReservationPossible res@(Reservation date _ _ requestedQuantity) reservationsOnDay =
  let
    availableSeats = availableCapacity reservationsOnDay
  in (availableSeats >= requestedQuantity)


    