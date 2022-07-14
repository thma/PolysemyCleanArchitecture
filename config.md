# Configuration of a Polysemy application

[![Actions Status](https://github.com/thma/PolysemyCleanArchitecture/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/PolysemyCleanArchitecture/actions)
<a href="https://github.com/thma/PolysemyCleanArchitecture"><img src="https://thma.github.io/img/forkme.png" height="20" ></a>


## Introduction

This is yet another sequel to my [Clean Architecture with Haskell and Polysemy article](https://thma.github.io/posts/2020-05-29-polysemy-clean-architecture.html).

In my last blog post I demonstrated how the actual hosting of a WAI web app (e.g. with the Warp server or on AWS Lambda) can be defined as a Polysemy effect.
The idea was well received on [reddit/r/haskell](https://www.reddit.com/r/haskell/) and also stirred some further discussions.
One question that came up: why did I explicitly load configuration before starting the Polysemy effect interpreter? 
Wouldn't it be much more in line with the overall idea of my *Polysemy Clean Architecture* to handle this loading also as an effect? 

Here is my original code:

```haskell
main :: IO ()
main = do
  config <- loadConfig       -- load config
  serveAppFromConfig config  -- create app from config and run it via AppServer effect
    & runWarpAppServer       -- use Warp to run rest application
    & runM
```

I explained my reasons for this design but promised to look for a better solution.

In the following I'll explain the new design that I came up with.

## The ideas behind my original design

The central reason for explicitely loading the configuration before starting the polysemy interpreter was that the configuration `config` is needed to select effect interpreter functions in the assembly of the WAI web app. To better understand this we'll drill down the execution sequence starting from `runWarpAppServer` effect handler:

```haskell
runWarpAppServer :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runWarpAppServer = interpret $ \case
  -- serving an application by constructing it from a config
  ServeAppFromConfig config ->
    embed $
      let app = createApp config
       in do 
        putStrLn $ "starting Warp on Port " ++ show (port config)
        Warp.run (port config) app

createApp :: Config -> Application
createApp config = serve reservationAPI (liftServer config)

liftServer :: Config -> ServerT ReservationAPI Handler
liftServer config = hoistServer reservationAPI (interpretServer config) reservationServer
  where
    interpretServer :: (Show k, Read k, ToJSON v, FromJSON v)
                    => Config -> Sem '[KVS k v, Input Config, Trace, Error ReservationError, Embed IO] a -> Handler a
    interpretServer config sem  =  sem
      & runSelectedKvsBackend config
      & runInputConst config
      & runSelectedTrace config
      & runError @ReservationError
      & runM
      & liftToHandler

    liftToHandler :: IO (Either ReservationError a) -> Handler a
    liftToHandler = Handler . ExceptT . fmap handleErrors

    handleErrors :: Either ReservationError b -> Either ServerError b
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value
```

The interesting point here is the usage of the `Config` parameter `config` in `interpretServer`. 
It is used to select concrete effect handler functions in `runSelectedKvsBackend conf` and `runSelectedTrace conf`:


```haskell
-- | global application configuration
data Config = Config {
  port        :: Int     -- ^ the port where the server is listening
, backend     :: Backend -- ^ selects the persistence backend for the KV store
, dbPath      :: String  -- ^ the path to the database
, verbose     :: Bool    -- ^ True enables logging
} deriving (Show, Eq, Read)

data Backend = SQLite | FileServer deriving (Show, Eq, Read)


-- | can select between SQLite or FileServer persistence backends.
runSelectedKvsBackend :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k, Read k, ToJSON v, FromJSON v)
                 => Config -> Sem (KVS k v : r) a -> Sem r a
runSelectedKvsBackend config = case backend config of
  SQLite     -> runKvsAsSQLite
  FileServer -> runKvsAsFileServer
  
-- | if the config flag verbose is set to True, trace to Console, else ignore all trace messages
runSelectedTrace :: (Member (Embed IO) r) => Config -> (Sem (Trace : r) a -> Sem r a)
runSelectedTrace config =
  if verbose config
    then traceToStdout
    else ignoreTrace
```

In addition to this `config` is also used by the `Input` effect handler `runInputConst conf`:

```haskell
-- | Run an 'Input' effect by always giving back the same value.
runInputConst :: i -> Sem (Input i ': r) a -> Sem r a
runInputConst c = interpret $ \case
  Input -> pure c
```

This allows effect handlers like `runKvsAsSQLite` to use `config` as configuration. For example to obtain the database connection:

```haskell
import           Polysemy
import           Polysemy.Input                 (Input, input)
import           Polysemy.Trace                 (Trace, trace)

-- | delete a value from db identified by key
deleteAction :: (Member (Input Config) r, Member (Embed IO) r, Member Trace r, Show k) => k -> Sem r ()
deleteAction key = do
  trace $ "deleteAction: " ++ show key
  conn <- connectionFrom input
  embed $ SQL.executeNamed conn "DELETE FROM store WHERE key = :key" [":key" := show key]


-- | create a connection based on configuration data, make sure table "store" exists.
connectionFrom :: (Member (Embed IO) r, Member Trace r) => Sem r Config -> Sem r SQL.Connection
connectionFrom c = do
  config <- c
  trace $ "open connection to: " ++ dbPath config
  embed (getConnection (dbPath config))
    where
      getConnection :: FilePath -> IO SQL.Connection
      getConnection dbFile = do
        conn <- SQL.open dbFile
        SQL.execute_ conn "CREATE TABLE IF NOT EXISTS store (key TEXT PRIMARY KEY, value TEXT)"
        return conn
```

My reasoning was as follows: As I needed `config` as an explicit parameter to `serveAppFromConfig` in the `main` glue code and `loadConfig`being of type `IO Config`, I thought I had to explicitely execute it in the `IO` Monad and then handing it into the Polysemy effect chain:

```haskell
main :: IO ()
main = do
  config <- loadConfig       -- load config
  serveAppFromConfig config  -- create app from config and run it via AppServer effect
    & runWarpAppServer       -- use Warp to run rest application
    & runM
```

But it turned out that I had just not thought things through deep enough!

## Chaining of Config loading and application execution as effects

If we take a step back and look at the code in the `UseCases` package we'll see that we already have shown how to combine different effects into a sequence within the Polysemy `Sem` Monad.
Take for example the following use case implementation:

```haskell
cancel :: (Member Persistence r, Member Trace r)  => Dom.Reservation -> Sem r ()
cancel res@(Dom.Reservation date _ _ _) = do
  trace $ "deleting reservation " ++ show res
  reservations <- fetch date
  trace $ "before: " ++ show reservations
  let after = Dom.cancelReservation res reservations
  trace $ "after: " ++ show after
  insertKvs date after
```

So instead of glueing stuff together in `main :: IO ()`, wouldn't it be much more in line with our overall intention to formulate the sequencing of configuration loading and hosting the WAI application as a sequence of Polysemy effects? For example:

```haskell
-- | load configuration via ConfigProvider effect, then contruct and run app via AppServer effect
configureAndServeApp ::  (Member ConfigProvider r, Member AppServer r)  => Sem r ()
configureAndServeApp = do
  config <- getConfig
  serveAppFromConfig config
```

In this function we use two effects `ConfigProvider` and `Appserver`. I already described the `AppServer` effect in [my previous blog post](https://thma.github.io/posts/2022-07-04-polysemy-and-warp.html). So we only have to consider the `ConfigProvider` effect here.


## Defining a ConfigProvider Effect

First we define the ConfigProvider Effect. It provides an effect function `getConfig :: Member ConfigProvider r => Sem r Config`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module ExternalInterfaces.ConfigProvider where

import InterfaceAdapters.Config
import Polysemy

-- | The ConfigProvider effect can be used to provide and application with a Config instance.
data ConfigProvider m a where
  GetConfig :: ConfigProvider m Config

-- makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
-- getConfig :: Member ConfigProvider r => Sem r Config
makeSem ''ConfigProvider
```

## Implementing the ConfigProvider effect

Next we define an implementation of the `ConfigProvider` effect by defining an effect handler function 
that loads a `Config` instance from a file:

```haskell
module ExternalInterfaces.FileConfigProvider where

import InterfaceAdapters.Config
import ExternalInterfaces.ConfigProvider
import Polysemy (Embed, Member, Sem, embed, interpret)

-- | provides a Config object from a local file path
runFileConfigProvider :: (Member (Embed IO) r) => FilePath -> Sem (ConfigProvider : r) a -> Sem r a
runFileConfigProvider path = interpret $ \case
  GetConfig -> embed $ loadConfig path


-- | load application config from file "application.config"
loadConfig :: FilePath -> IO Config
loadConfig path = do
  input <- readFile path
  pure $ read input
```

## The new and shining main function

Now we have all the ingredients ready to clean up the glue code in `main :: IO ()`:

```haskell
main :: IO ()
main = do
  configureAndServeApp
    & runFileConfigProvider "application.config"  -- provide Config from a file
    & runWarpAppServer                            -- use Warp to run rest application
    & runM
```

The complete control of the application is now exclusively managed by the Polysemy effect library.

## Conclusion

I'm excited about how the comments on my last blog post have helped me develop the
Polysemy Clean Architecture idea into a much improved design.

So I'm sure that this post will trigger some more discussions and will help to improve remaining grey spots in the
overall concept.



