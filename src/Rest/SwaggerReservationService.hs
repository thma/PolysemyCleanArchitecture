{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
module Rest.SwaggerReservationService where

import           Control.Lens
import           Data.Aeson                   (toJSON)
import           Data.Swagger
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI
import           System.Process               (createProcess, shell, ProcessHandle)
import           GHC.IO.Handle.Types          (Handle)
import           System.Info                  (os)

import           Rest.ReservationService      (ReservationAPI, reservationAPI, reservationServer)
import           BL.ReservationBusinessLogic  (Reservation (..))

-- | Swagger spec of Model type 'Reservation'
instance ToSchema Reservation where
    declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped.schema.description ?~ "This is a Reservation API"
      & mapped.schema.example ?~ toJSON (Reservation (read "2020-01-29") "Max Muster" "mm@muster.com" 4)

-- | Swagger spec for user API.
swaggerDoc :: Swagger
swaggerDoc = toSwagger reservationAPI
    & info.title   .~ "Reservation API"
    & info.version .~ "0.1"
    & info.description ?~ "This is an API that provides restaurant servervations"
    & info.license     ?~ ("APACHE 2.0" & url ?~ URL "http://apache.org")

-- | API type with bells and whistles, i.e. schema file and swagger-ui.
type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> ReservationAPI

-- | boilerplate to guide type inference
api :: Proxy API
api = Proxy

-- | Servant server for an API
server :: Server API
server = 
  swaggerSchemaUIServer 
    swaggerDoc :<|> reservationServer

-- | 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve api server

-- | start up server and launch browser on swagger UI
up :: IO ()
up = do
  let port = 8080
  putStrLn $ "GET all reservation: http://localhost:" ++ show port ++ "/reservations"
  putStrLn $ "Swagger UI:    http://localhost:" ++ show port ++ "/swagger-ui"
  launchSiteInBrowser port
  run port app


-- | convenience function that opens the swagger UI in the default web browser
launchSiteInBrowser:: Int -> IO ()
launchSiteInBrowser port = do
  case os of
    "mingw32" -> createProcess  (shell $ "start "    ++ url)
    "darwin"  -> createProcess  (shell $ "open "     ++ url)
    _         -> createProcess  (shell $ "xdg-open " ++ url)
  return ()
  where 
    url = "http://localhost:" ++ show port ++ "/swagger-ui"