{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments       #-}
module Rest.ReservationService where

import           Servant
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString.Lazy.Char8 (pack)

import qualified BL.ReservationBusinessLogic as BL
import qualified Persistence.ReservationTablePersistence as RTP
import           Persistence.DatasourceFileImpl

-- | REST api for Restaurant Reservations
type ReservationAPI =
       "reservations" :> Summary "retrieve all reservations"
                      :> Get     '[ JSON] BL.ReservationTable
  :<|> "reservations" :> Summary "place a new reservation"
                      :> ReqBody '[ JSON] BL.Reservation
                      :> Post    '[ JSON] ()


-- | implements the ReservationAPI
reservationServer :: Server ReservationAPI
reservationServer =
        getAllReservations   -- GET  /reservations
  :<|>  postReservation      -- POST /reservations


-- | get the reservation table
getAllReservations :: Handler BL.ReservationTable
getAllReservations = do
  liftIO $ putStrLn "GET /reservations"
  liftIO $ RTP.loadTable BL.restaurant

-- | post a reservation
postReservation :: BL.Reservation -> Handler ()
postReservation reservation = do
  liftIO $ putStrLn $ "POST /reservations/ " ++ show reservation
  -- load reservation table from persistent storage
  table <- liftIO $ RTP.loadTable BL.restaurant
  -- try to place new reservation
  case BL.tryReservation table reservation BL.maxCapacity  of
    -- if successful: persist the updated reservation table
    Just reservationTable -> liftIO (RTP.storeTable (return reservationTable) BL.restaurant)
    -- else: signal 412 error to the client
    Nothing -> throwError err412 {errBody = pack "\"Capacity exceeded. Sorry we are already fully booked on this date!\""}

-- | boilerplate needed to guide type inference
reservationAPI :: Proxy ReservationAPI
reservationAPI = Proxy

-- | 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver. 
-- Useful for testing.
app :: Application
app = serve reservationAPI reservationServer