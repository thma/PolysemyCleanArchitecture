{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments       #-}
module External.ReservationRestService where

import           Servant
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Lazy.Char8 (pack)
import           Polysemy
import           Polysemy.Error

import qualified UseCases.ReservationUseCase as UC (ReservationTable, ReservationError, listAll, tryReservation, fetch, cancel)
import qualified Domain.ReservationDomain    as Dom (Reservation, ReservationMap)
import Polysemy.Trace (Trace)
import Polysemy.Input (Input)
import UseCases.Config (Config)
import Data.Time.Calendar (Day)
import Control.Error (fromMaybe)

-- | REST api for Restaurant Reservations
type ReservationAPI =
       "reservations" :> Summary "retrieve all reservations"
                      :> Get     '[ JSON] Dom.ReservationMap
                      
  :<|> "reservations" :> Summary "retrieve list of reservations for a given day"
                      :> Capture "day" Day
                      :> Get     '[ JSON] (Maybe [Dom.Reservation])

  :<|> "reservations" :> Summary "place a new reservation"
                      :> ReqBody '[ JSON] Dom.Reservation
                      :> Post    '[ JSON] ()

-- | implements the ReservationAPI
reservationServer :: (Member UC.ReservationTable r, Member (Error UC.ReservationError) r, Member Trace r, Member (Input Config) r) => ServerT ReservationAPI (Sem r)
reservationServer =
        UC.listAll        -- GET  /reservations
  :<|>  UC.fetch          -- GET  /reservations/YYYY-MM-DD  
  :<|>  UC.tryReservation -- POST /reservations

-- | boilerplate needed to guide type inference
reservationAPI :: Proxy ReservationAPI
reservationAPI = Proxy



