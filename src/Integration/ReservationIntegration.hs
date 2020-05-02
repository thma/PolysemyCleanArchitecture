module Integration.ReservationIntegration
( listAll
, fetch
, tryReservation
, ReservationTable (..)
, ReservationError (..)
, Reservation (..)
, ReservationMap (..)
)
where

--import           Data.Aeson      --(FromJSON, ToJSON)
--import           GHC.Generics
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Data.Map.Strict as M
import Polysemy
import Polysemy.Error

import Integration.KVS
import Domain.ReservationBusinessLogic

{--
This module implements the integration layer for the Reservation system.
It coordinates access to Effects and domain logic.
The module exposes service function that will be used by the REST API.

All Effects are specified as Polysemy Members.

Interpretation of Effects is implemented on the level of application assembly.
--}

-- | ReservationTable holds a list of reservations for each date
type ReservationTable = KVS Day [Reservation]

data ReservationError = ReservationNotPossible String

type ReservationMap = M.Map Day [Reservation]

listAll :: Member ReservationTable r => Sem r (ReservationMap)
listAll = fmap M.fromList listAllKvs

fetch :: Member ReservationTable r => Day -> Sem r (Maybe [Reservation])
fetch = getKvs

-- | try to add a reservation to the table.
-- | Return Just the modified table if successful, else return Nothing
tryReservation :: (Member ReservationTable r, Member (Error ReservationError) r) => Reservation -> Sem r ()
tryReservation res@(Reservation date _ _ requestedQuantity)  = do
  reservationsOnDay <- getKvs date
  let availableSeats = availableCapacity reservationsOnDay
  if availableSeats >= requestedQuantity
    then
      do addReservation res
         return ()
    else throw $ ReservationNotPossible ("we are fully booked on " ++ show date)

-- | add a reservation to the reservation table
addReservation :: (Member (KVS Day [Reservation]) r)  => Reservation -> Sem r [Reservation]
addReservation x@(Reservation date _ _ _ ) = do
  resList <- getKvs date
  let reserved = case resList of
        Nothing -> [x]
        Just xs -> x:xs
  insertKvs date reserved
  return reserved
