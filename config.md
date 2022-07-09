# Configuration of a Polysemy application

[![Actions Status](https://github.com/thma/PolysemyCleanArchitecture/workflows/Haskell%20CI/badge.svg)](https://github.com/thma/PolysemyCleanArchitecture/actions)
<a href="https://github.com/thma/PolysemyCleanArchitecture"><img src="https://thma.github.io/img/forkme.png" height="20" ></a>

## tl;dr

Let's define two Polysemy effects to transparently handle application configuration:
Loading configuration, using configuration to select effect interpreter functions as well as configuration of effects is all done by a single mechanism.

[here is the code](https://github.com/thma/PolysemyCleanArchitecture)

## Introduction

In my last blog post I demonstrated how the actual hosting of a WAI web app (e.g. with the Warp server or on AWS Lambda) can be defined as a Polysemy effect.
The idea was well received on [reddit/r/haskell](https://www.reddit.com/r/haskell/) and also stirred some further discussions.
One question that came up: why did I explicitly load configuration before starting the Polysemy effect interpreter? Wouldn't it be much more in line with the overall idea of my *Polysemy Clean Architecture* to handle this loading also as an effect? Here is my original code:

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

My reasoning was as follows: As I needed `config` as an explicit parameter to `serveAppFromConfig` in the `main` glue code, I thought I had to explicitely load it before entering the Polysemy effect chain:

```haskell
main :: IO ()
main = do
  config <- loadConfig       -- load config
  serveAppFromConfig config  -- create app from config and run it via AppServer effect
    & runWarpAppServer       -- use Warp to run rest application
    & runM
```

But it turned out that I had just not thought it through deep enough!

## Chaining of Config loading and application execution as effects

Instead of glueing stuff together in `main :: IO ()`, wouldn't it be much more in line with our overall intention formulate the sequencing of configuration loading and hosting the WAI application as a sequence
of Polysemy effects? Maybe something like:

```haskell
-- | load configuration via ConfigProvider effect, then contruct and run app via AppServer effect
configureAndServeApp ::  ( Member ConfigProvider r, Member AppServer r)  => Sem r ()
configureAndServeApp = do
  config <- getConfig
  serveAppFromConfig config
```

In this function we use two effects `ConfigProvider` and `Appserver`. 


## Defining a ConfigProvider Effect

## A simple file based ConfigProvider effect handler

## Chaining ConfigProvider and AppServer effects

## The new and shining glue code
