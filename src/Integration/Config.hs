module Integration.Config where

data Config = Config {
  maxCapacity :: Int -- ^ the max number of Seats in the Restaurant
, port        :: Int -- ^ the port where the server is listening
}


