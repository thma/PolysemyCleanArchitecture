{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module InterfaceAdapters.ReservationRestService where

import           Control.Error               (fromMaybe)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Aeson.Types            (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8  (pack)
import           Data.Time.Calendar          (Day)
import qualified Domain.ReservationDomain    as Dom (Reservation,
                                                     ReservationMap)
import           InterfaceAdapters.Config   (Config)
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input              (Input)
import           Polysemy.Trace              (Trace)
import           Servant
import qualified UseCases.ReservationUseCase as UC (ReservationError,
                                                    Persistence,
                                                    availableSeats, cancel,
                                                    fetch, listAll,
                                                    tryReservation)
                                                    
-- | in order to allow JSON serialization for the Dom.Reservation type, it must instantiate FromJSON and ToJSON.
instance ToJSON Dom.Reservation
instance FromJSON Dom.Reservation

-- | Declaring the routes of the REST API for Restaurant Reservations
type ReservationAPI =
       "reservations" :> Summary "retrieve a map of all reservations (Day -> [Reservation])"
                      :> Get     '[ JSON] Dom.ReservationMap -- GET    /reservations

  :<|> "reservations" :> Summary "retrieve list of reservations for a given day"
                      :> Capture "day" Day
                      :> Get     '[ JSON] [Dom.Reservation]  -- GET    /reservations/YYYY-MM-DD

  :<|> "reservations" :> Summary "place a new reservation"
                      :> ReqBody '[ JSON] Dom.Reservation
                      :> Post    '[ JSON] ()                 -- POST   /reservations

  :<|> "reservations" :> Summary "cancel a reservation"
                      :> ReqBody '[ JSON] Dom.Reservation
                      :> Delete  '[ JSON] ()                 -- DELETE /reservations
                      
  :<|> "seats"        :> Summary "retrieve number of free seats for a given day"
                      :> Capture "day" Day
                      :> Get     '[ JSON] Natural            -- GET    /seats/YYYY-MM-DD

-- | implements the ReservationAPI
reservationServer :: (Member UC.Persistence r, Member (Error UC.ReservationError) r, 
                      Member Trace r, Member (Input Config) r) => ServerT ReservationAPI (Sem r)
reservationServer =
        UC.listAll        -- GET    /reservations
  :<|>  UC.fetch          -- GET    /reservations/YYYY-MM-DD
  :<|>  UC.tryReservation -- POST   /reservations
  :<|>  UC.cancel         -- DELETE /reservations
  :<|>  UC.availableSeats -- GET    /seats/YYYY-MM-DD

-- | boilerplate needed to guide type inference
reservationAPI :: Proxy ReservationAPI
reservationAPI = Proxy



