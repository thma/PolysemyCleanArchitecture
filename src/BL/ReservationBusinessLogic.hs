{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module BL.ReservationBusinessLogic where

import           Data.Aeson      --(FromJSON, ToJSON)
import           GHC.Generics
import           Data.Time.Calendar
import           Data.Time.Clock
--import           Control.Monad
import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error

import BL.KVS

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
type ReservationTable = KVS Day [Reservation]

type ReservationMap = M.Map Day [Reservation]

-- | the total number of seats in the restaurant
maxCapacity :: Int
maxCapacity = 20   

list :: Member ReservationTable r => Sem r (M.Map Day [Reservation])
list = fmap M.fromList listAllKvs

fetch :: Member ReservationTable r => Day -> Sem r (Maybe [Reservation])
fetch = getKvs
  
data ReservationError = ReservationNotPossible String

-- | computes the number of available seats for a given date
availableCapacity :: Member ReservationTable r => Day -> Int -> Sem r Int
availableCapacity day totalCapacity = do
  reservationsOnDay <- getKvs day
  case reservationsOnDay of
    Nothing  -> return totalCapacity
    Just res -> return $ totalCapacity - usedCapacity res

-- | computes the number of reserved seats for a list of reservations
usedCapacity :: [Reservation] -> Int
usedCapacity [] = 0
usedCapacity (Reservation _ _ _ quantity : rest) = quantity + usedCapacity rest

-- | try to add a reservation to the table.
-- | Return Just the modified table if successful, else return Nothing
--tryReservation :: ReservationTable -> Reservation -> Int -> Maybe ReservationTable
tryReservation :: (Member ReservationTable r, 
                   Member (Error ReservationError) r) => Reservation -> Sem r ()
tryReservation res@(Reservation date _ _ requestedQuantity)  = do
  availableSeats <- availableCapacity date maxCapacity
  if availableSeats >= requestedQuantity
    then 
      do addReservation res
         return ()
    else throw $ ReservationNotPossible "we are fully booked"

-- | add a reservation to the reservation table
addReservation :: (Member (KVS Day [Reservation]) r)  => Reservation -> Sem r [Reservation]
addReservation x@(Reservation date _ _ _ ) = do
  resList <- getKvs date
  let reserved = case resList of
        Nothing -> [x]
        Just xs -> x:xs
  insertKvs date reserved
  return reserved
    