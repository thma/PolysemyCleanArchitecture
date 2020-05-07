module Integration.ReservationIntegration
( listAll
, fetch
, tryReservation
, ReservationTable (..)
, ReservationError (..)
, D.Reservation (..)
, ReservationMap (..)
)
where

import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Polysemy
import           Polysemy.Error

import qualified Domain.ReservationBusinessLogic as D (Reservation (..), isReservationPossible, addReservation)
import           Effects.KVS                 (KVS, getKvs, insertKvs, listAllKvs)
import           Polysemy.Trace                  (Trace, trace)

{--
This module specifies the integration layer for the Reservation system.
It coordinates access to Effects and the actual domain logic.
The module exposes service function that will be used by the REST API.

All Effects are specified as Polysemy Members.

Interpretation of Effects is implemented on the level of application assembly, or in the context of unit tests.

Please note: all functions in this module are pure and total functions.
This makes it easy to test them in isolation.

--}

-- | ReservationTable holds a list of reservations for each date
type ReservationTable = KVS Day [D.Reservation]

newtype ReservationError = ReservationNotPossible String deriving (Show, Eq)

type ReservationMap = M.Map Day [D.Reservation]

-- | list all entries from the key value store and return them as a ReservationMap
listAll :: (Member ReservationTable r, Member Trace r) => Sem r ReservationMap
listAll = do
  trace "listing all reservation entries"
  fmap M.fromList listAllKvs

-- | fetch the list of reservations for a given day from the key value store.
--   If no match is found, Nothings is returned, else the Result wrapped with Just.
fetch :: (Member ReservationTable r, Member Trace r) => Day -> Sem r (Maybe [D.Reservation])
fetch day = do
  trace $ "fetch reservations for " ++ show day
  getKvs day

-- | try to add a reservation to the table.
-- | Return Just the modified table if successful, else return Nothing
tryReservation :: (Member ReservationTable r, Member (Error ReservationError) r, Member Trace r) => D.Reservation -> Sem r ()
tryReservation res@(D.Reservation date _ _ requestedQuantity)  = do
  trace $ "trying to reservate " ++ show requestedQuantity ++ " more seats on " ++ show date
  maybeReservations <- fetch date
  let reservationsOnDay = fromMaybe [] maybeReservations
  if D.isReservationPossible res reservationsOnDay
    then persistReservation res
    else throw $ ReservationNotPossible ("we are fully booked on " ++ show date)

-- | persist a reservation to the reservation table.
persistReservation :: (Member (KVS Day [D.Reservation]) r, Member Trace r)  => D.Reservation -> Sem r ()
persistReservation r@(D.Reservation date _ _ _ ) = do
  trace $ "enter a new reservation to KV store: " ++ show r
  maybeResList <- getKvs date
  let rs = fromMaybe [] maybeResList
  insertKvs date (D.addReservation r rs)
