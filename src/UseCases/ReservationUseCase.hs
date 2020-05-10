module UseCases.ReservationUseCase
( listAll
, fetch
, tryReservation
, cancel
, ReservationTable (..)
, ReservationError (..)
, Dom.Reservation (..)
, Dom.ReservationMap (..)
)
where

import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace              (Trace, trace)
import           Polysemy.Input              (Input, input)

import qualified Domain.ReservationDomain as Dom (Reservation (..), ReservationMap (..), isReservationPossible, addReservation, cancelReservation, availableSeats)
import           UseCases.KVS               (KVS, getKvs, insertKvs, listAllKvs)
import           UseCases.Config
import           Control.Monad (when)

{--
This module specifies the Use Case layer for the Reservation system.
It coordinates access to Effects and the actual domain logic.
The module exposes service functions that will be used by the REST API in the External layer.

Implemented Use Cases:

1. Display the list of reservations for a given day.

2. Enter a reservation for a given day and keep it persistent.
   If the reservation can not be served as all seats are occupies prode a functional error message stating
   the issue.
   
3. Delete a given reservation from the system in case of a cancellation.
   NO functional error is required if the reservation is not present in the system.

4. Display a List of all reservation in the system.


All Effects are specified as Polysemy Members.

Interpretation of Effects is implemented on the level of application assembly, or in the context of unit tests.

Please note: all functions in this module are pure and total functions.
This makes it easy to test them in isolation.
--}

-- | ReservationTable holds a list of reservations for each date
type ReservationTable = KVS Day [Dom.Reservation]

-- | The functional error, raised if a reservation is not possible
newtype ReservationError = ReservationNotPossible String deriving (Show, Eq)


-- | fetch the list of reservations for a given day from the key value store.
-- | If no match is found, Nothings is returned, else the Result wrapped with Just.
-- | Implements UseCase 1.
fetch :: (Member ReservationTable r, Member Trace r) => Day -> Sem r [Dom.Reservation]
fetch day = do
  trace $ "fetch reservations for " ++ show day
  maybeList <- getKvs day
  return $ fromMaybe [] maybeList


-- | try to add a reservation to the table.
-- | Return Just the modified table if successful, else return Nothing
-- | implements UseCase 2.
tryReservation :: (Member ReservationTable r, Member (Error ReservationError) r, Member Trace r, Member (Input Config) r) => Dom.Reservation -> Sem r ()
tryReservation res@(Dom.Reservation date _ _ requestedQuantity)  = do
  trace $ "trying to reservate " ++ show requestedQuantity ++ " more seats on " ++ show date
  todaysReservations <- fetch date
  config <- input
  let totalCapacity = maxCapacity config
      availableSeats = Dom.availableSeats totalCapacity todaysReservations
  if Dom.isReservationPossible res todaysReservations totalCapacity
    then persistReservation res
    else throw $ ReservationNotPossible ("Sorry, only " ++ show availableSeats ++ " seats left on " ++ show date)
    
  where
    -- | persist a reservation to the reservation table.
    persistReservation :: (Member (KVS Day [Dom.Reservation]) r, Member Trace r)  => Dom.Reservation -> Sem r ()
    persistReservation r@(Dom.Reservation date _ _ _ ) = do
      trace $ "enter a new reservation to KV store: " ++ show r
      rs <- fetch date
      insertKvs date (Dom.addReservation r rs)


-- | cancel a reservation, that is: delete it from the system.
-- | Implements UseCase 3.
cancel :: (Member (KVS Day [Dom.Reservation]) r, Member Trace r)  => Dom.Reservation -> Sem r ()
cancel res@(Dom.Reservation date _ _ _) = do
  trace $ "deleting reservation " ++ show res
  reservations <- fetch date
  trace $ "before: " ++ show reservations
  let after = Dom.cancelReservation res reservations
  trace $ "after: " ++ show after
  insertKvs date after
      
  
-- | list all entries from the key value store and return them as a ReservationMap
-- | Implements UseCase 4.
listAll :: (Member ReservationTable r, Member Trace r) => Sem r Dom.ReservationMap
listAll = do
  trace "listing all reservation entries"
  fmap M.fromList listAllKvs    


