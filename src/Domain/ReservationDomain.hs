{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Domain.ReservationDomain
( Reservation (..)
, ReservationMap (..)
, usedCapacity
, availableSeats
, isReservationPossible
, addReservation
, cancelReservation
)

where

import           Data.Aeson
import           Data.List
import qualified Data.Map.Strict    as M
import           Data.Time.Calendar
import           Data.Time.Clock
import           GHC.Generics

{--
This module implements the business logic for seat reservations in a very small boutique restaurant.
The restaurant has only one table with 20 tables.
Each day the restaurants accepts only 20 seat reservations. (There is no limited time-slot for each guest.)

Please note: all functions in this module are pure and total.
This makes it easy to test them in isolation.
--}

-- | a data type representing a reservation
data Reservation = Reservation
    { date     :: Day    -- ^ the date of the reservation
    , name     :: String -- ^ the name of the guest placing the reservation
    , email    :: String -- ^ the email address of the guest
    , quantity :: Int    -- ^ how many seats are requested
    }
    deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | a key value map holding a list of reservations for any given day
type ReservationMap = M.Map Day [Reservation]

-- | computes the number of reserved seats for a list of reservations
usedCapacity :: [Reservation] -> Int
usedCapacity [] = 0
usedCapacity (Reservation _ _ _ quantity : rest) = quantity + usedCapacity rest

-- | check whether it is possible to add a reservation to the table.
-- | Return True if successful, else return False
isReservationPossible :: Reservation -> [Reservation] -> Int -> Bool
isReservationPossible res@(Reservation date _ _ requestedSeats) reservationsOnDay maxCapacity =
  availableSeats maxCapacity reservationsOnDay >= requestedSeats

-- | computes the number of available seats for a list of reservations.
availableSeats maxCapacity reservationsOnDay= maxCapacity - usedCapacity reservationsOnDay

-- | add a reservation to
addReservation :: Reservation -> [Reservation] -> [Reservation]
addReservation r rs = r:rs

-- | cancel a reservation for a given list of reservations.
cancelReservation :: Reservation -> [Reservation] -> [Reservation]
cancelReservation = delete
