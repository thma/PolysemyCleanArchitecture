{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module BL.ReservationBusinessLogic where

import           Data.Aeson      --(FromJSON, ToJSON)
import           GHC.Generics
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Map (Map)
import qualified Data.Map as Map

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

-- | ReservationTable holds a list of reservations for each date
type ReservationTable = Map Day [Reservation]

-- | the total number of seats in the restaurant
maxCapacity :: Int
maxCapacity = 20   

-- | The Restaurant name
restaurant :: String
restaurant = "My Mum Made" 

-- | computes the number of available seats for a given date
availableCapacity :: ReservationTable -> Day -> Int -> Int
availableCapacity table day totalCapacity =
  case Map.lookup day table of
    Nothing  -> totalCapacity
    Just res -> totalCapacity - usedCapacity res

-- | computes the number of reserved seats for a list of reservations
usedCapacity :: [Reservation] -> Int
usedCapacity [] = 0
usedCapacity (Reservation _ _ _ quantity : rest) = quantity + usedCapacity rest

-- | try to add a reservation to the table.
-- | Return Just the modified table if successful, else return Nothing
tryReservation :: ReservationTable -> Reservation -> Int -> Maybe ReservationTable
tryReservation table res@(Reservation date _ _ requestedQuantity) totalCapacity =
  if availableCapacity table date totalCapacity >= requestedQuantity
    then Just (addReservation table res)
    else Nothing

-- | add a reservation to the reservation table
addReservation :: ReservationTable -> Reservation -> ReservationTable
addReservation table reservation@(Reservation date _ _ _ ) = Map.alter matchAction date table
  where
    matchAction :: Maybe [Reservation] -> Maybe [Reservation]
    matchAction Nothing             = Just [reservation]
    matchAction (Just reservations) = Just $ reservation : reservations

