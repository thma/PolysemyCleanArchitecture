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
import           Data.Maybe (fromMaybe)
import Polysemy
import Polysemy.Error

import Integration.KVS (KVS, listAllKvs, getKvs, insertKvs)
import Domain.ReservationBusinessLogic (Reservation (..), isReservationPossible)
import Polysemy.Trace (Trace, trace)

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

-- | list all entries from the key value store and return them as a ReservationMap
listAll :: (Member ReservationTable r, Member Trace r) => Sem r (ReservationMap)
listAll = do
  trace "listing all reservation entries"
  result <- fmap M.fromList listAllKvs
  trace (show result)
  return result

-- | fetch the list of reservations for a given day from the key value store.
--   If no match is found, Nothings is returned, else the Result wrapped with Just. 
fetch :: (Member ReservationTable r, Member Trace r) => Day -> Sem r (Maybe [Reservation])
fetch day = do
  trace $ "fetch reservations for " ++ show day
  getKvs day

-- | try to add a reservation to the table.
-- | Return Just the modified table if successful, else return Nothing
tryReservation :: (Member ReservationTable r, Member (Error ReservationError) r, Member Trace r) => Reservation -> Sem r ()
tryReservation res@(Reservation date _ _ requestedQuantity)  = do
  trace $ "trying to reservate " ++ show requestedQuantity ++ " more seats on " ++ show date
  maybeReservations <- fetch date  
  let reservationsOnDay = fromMaybe [] maybeReservations
  if isReservationPossible res reservationsOnDay
    then addReservation res
    else throw $ ReservationNotPossible ("we are fully booked on " ++ show date)

-- | add a reservation to the reservation table.
addReservation :: (Member (KVS Day [Reservation]) r, Member Trace r)  => Reservation -> Sem r ()
addReservation x@(Reservation date _ _ _ ) = do
  trace $ "enter a new reservation to KV store: " ++ show x
  resList <- getKvs date
  let reserved = case resList of
        Nothing -> [x]
        Just xs -> x:xs
  insertKvs date reserved
  return ()
