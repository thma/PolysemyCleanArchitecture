{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.ReservationDomain
  ( Reservation (..),
    ReservationMap,
    usedCapacity,
    availableSeats,
    isReservationPossible,
    addReservation,
    cancelReservation,
  )
where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.List (delete)
import qualified Data.Map.Strict as M
import Data.Time.Calendar
import GHC.Generics
import Numeric.Natural

{--
This module implements the business logic for seat reservations in a very small boutique restaurant.
The restaurant has only one table with 20 seats.
Each day the restaurants accepts only 20 seat reservations. (There is no limited time-slot for each guest.)

Please note: all functions in this module are pure and total.
This makes it easy to test them in isolation.
--}

-- | a data type representing a reservation
data Reservation = Reservation
  { -- | the date of the reservation
    date :: Day,
    -- | the name of the guest placing the reservation
    name :: String,
    -- | the email address of the guest
    email :: String,
    -- | how many seats are requested
    quantity :: Natural
  }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

-- | a key value map holding a list of reservations for any given day
type ReservationMap = M.Map Day [Reservation]

-- | computes the number of reserved seats for a list of reservations
usedCapacity :: [Reservation] -> Natural
usedCapacity [] = 0
usedCapacity (Reservation _ _ _ requested : rest) = requested + usedCapacity rest

-- | check whether it is possible to add a reservation to the table.
-- | Return True if successful, else return False
isReservationPossible :: Reservation -> [Reservation] -> Natural -> Bool
isReservationPossible (Reservation _ _ _ requestedSeats) reservationsOnDay maxCapacity =
  availableSeats maxCapacity reservationsOnDay >= requestedSeats

-- | computes the number of available seats from a maximum capacity and a list of reservations.
availableSeats :: Natural -> [Reservation] -> Natural
availableSeats maxCapacity reservations =
  if maxCapacity < usedCapacity reservations
    then 0
    else maxCapacity - usedCapacity reservations

-- | add a reservation to
addReservation :: Reservation -> [Reservation] -> [Reservation]
addReservation r rs = r : rs

-- | cancel a reservation for a given list of reservations.
cancelReservation :: Reservation -> [Reservation] -> [Reservation]
cancelReservation = delete
