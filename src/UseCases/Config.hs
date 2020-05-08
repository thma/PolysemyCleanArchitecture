module UseCases.Config where

-- | global application configuration
data Config = Config {
  maxCapacity :: Int -- ^ the max number of Seats in the Restaurant
, port        :: Int -- ^ the port where the server is listening
, dbPath      :: String -- ^ the path to the database
}


