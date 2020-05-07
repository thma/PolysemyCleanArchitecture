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

import qualified UseCases.ReservationIntegration as Int
import qualified Domain.ReservationDomain        as Dom
import Polysemy.Trace (Trace)
import Polysemy.Input (Input)
import UseCases.Config (Config)

-- | REST api for Restaurant Reservations
type ReservationAPI =
       "reservations" :> Summary "retrieve all reservations"
                      :> Get     '[ JSON] Dom.ReservationMap
  :<|> "reservations" :> Summary "place a new reservation"
                      :> ReqBody '[ JSON] Dom.Reservation
                      :> Post    '[ JSON] ()

-- | implements the ReservationAPI
reservationServer :: (Member Int.ReservationTable r, Member (Error Int.ReservationError) r, Member Trace r, Member (Input Config) r) => ServerT ReservationAPI (Sem r)
reservationServer =
        Int.listAll        -- GET  /reservations
  :<|>  Int.tryReservation -- POST /reservations

-- | boilerplate needed to guide type inference
reservationAPI :: Proxy ReservationAPI
reservationAPI = Proxy
