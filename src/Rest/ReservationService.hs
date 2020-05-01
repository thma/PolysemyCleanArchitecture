{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments       #-}
module Rest.ReservationService where

import           Servant
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Lazy.Char8 (pack)
import           Polysemy
import           Polysemy.Error

import qualified BL.ReservationBusinessLogic as BL

-- | REST api for Restaurant Reservations
type ReservationAPI =
       "reservations" :> Summary "retrieve all reservations"
                      :> Get     '[ JSON] BL.ReservationMap
  :<|> "reservations" :> Summary "place a new reservation"
                      :> ReqBody '[ JSON] BL.Reservation
                      :> Post    '[ JSON] ()

-- | implements the ReservationAPI
reservationServer :: ( Member BL.ReservationTable r, Member (Error BL.ReservationError) r) => ServerT ReservationAPI (Sem r)
reservationServer =
        BL.list           -- GET  /reservations
  :<|>  BL.tryReservation -- POST /reservations

-- | boilerplate needed to guide type inference
reservationAPI :: Proxy ReservationAPI
reservationAPI = Proxy
