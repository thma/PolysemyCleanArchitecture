# warp as an effect

## Introduction

A few months ago an interesting question was raised in the [issues section of my PolysemyCleanArchitecture GitHub project](https://github.com/thma/PolysemyCleanArchitecture/issues/2):

> I'm wondering if we want to change the server to serverless, is it easy to do that? 
> If we treat the server as an effect, can we just change the [polysemy effect] interpreter?

This question points to one of the grey spots of my original design: The whole polysemy effect stack is executed as part of the `createApp` function which provides an `Servant.Server.Application` which is then executed by `Warp.run`.

Wouldn't it be much more elegant to also define the server execution as an effect? This would make the overall design more homogenous by giving the final control to the Polysemy effect interpreter. 

In this post I'm going to present a design that provides a solution for this.

## The 'old' way

In order to better understand the issue lets have a brieve look at the original design.

```haskell
main :: IO ()
main = do
  config <- loadConfig           -- load configuration (e.g. from file)
  let p = port config            -- obtain port from config
  putStrLn $ "Starting server on port " ++ show p
  Warp.run p (createApp config)  -- create Application based on config and run it
```

As we can see from the `main` function our whole app is just an ordinary `Network.Wai.Application` which is executed by `Warp.run`.

So the final control of this application is managed by warp. We don't see anything of Polysemy here. Only if we dig deeper we see that the Polysemy effect stack is interpreted somewhere under the hood of the `Application`:

```haskell
createApp :: Config -> Application
createApp config = serve reservationAPI (liftServer config)


liftServer :: Config -> ServerT ReservationAPI Handler
liftServer config = hoistServer reservationAPI (interpretServer config) reservationServer
  where
    interpretServer :: (Show k, Read k, ToJSON v, FromJSON v)
                    => Config -> Sem '[KVS k v, Input Config, Trace, Error ReservationError, Embed IO] a -> Handler a
    interpretServer conf sem  =  sem
          & runSelectedKvsBackend conf
          & runInputConst conf
          & runSelectedTrace conf
          & runError @ReservationError
          & runM
          & liftToHandler

    liftToHandler :: IO (Either ReservationError a) -> Handler a
    liftToHandler = Handler . ExceptT . fmap handleErrors

    handleErrors :: Either ReservationError b -> Either ServerError b
    handleErrors (Left (ReservationNotPossible msg)) = Left err412 { errBody = pack msg}
    handleErrors (Right value) = Right value
```

This design works perfectly fine and also follows the common practise to exploit the Servant [`hoistServer`](https://hackage.haskell.org/package/servant-server-0.19.1/docs/Servant-Server.html#v:hoistServer) mechanism.

The only complaint is that the Polysemy effect interpreter is not executed as the outmost piece of code. Accordingly we have to deploy our application into warp rather than having Polysemy in control and executing warp as an effect.


## The new solution

Ok let's take this challenge and try to fix this 'bug'!

My overall approach goes like this:

1. Define a new `ExternalInterfaces.AppServer` effect. This will allow to abstractly define
   usage of an application server as a Polysemy effect.

2. Provide a Warp based implementation ExternalInterfaces.WarpAppServer.
   This will interprete the `AppServer` effect by running [Warp](http://www.aosabook.org/en/posa/warp.html).

3. write a main function that drives the whole Polysemy effect stack, including the Warp server. 
   
### The AppServer effect
```haskell
{-# LANGUAGE TemplateHaskell #-}

module ExternalInterfaces.AppServer where

import           InterfaceAdapters.Config
import           Network.Wai              (Application)
import           Polysemy                 (makeSem)

data AppServer m a where
  ServeApp           :: Int -> Application -> AppServer m ()    --^ serve a given application on a port
  ServeAppFromConfig :: Config -> AppServer m ()                --^ construct an application from a configuration and serve it


-- using TemplateHaskell to generate serveApp and serveAppFromConfig effect functions
makeSem ''AppServer
```

### The Warp based implementation of the effect

```haskell
module ExternalInterfaces.WarpAppServer where

import           ExternalInterfaces.AppServer
import           ExternalInterfaces.ApplicationAssembly (createApp, loadConfig)
import           InterfaceAdapters.Config               (Config (..))
import qualified Network.Wai.Handler.Warp               as Warp (run)
import           Polysemy                               (Embed, Member, Sem,
                                                         embed, interpret, runM)

-- | Warp Based implementation of AppServer
runWarpAppServer :: (Member (Embed IO) r) => Sem (AppServer : r) a -> Sem r a
runWarpAppServer = interpret $ \case
  -- this is the more generic version which maps directly to Warp.run
  ServeApp port app -> embed $ Warp.run port app

  -- serving an application by constructing it from a config
  ServeAppFromConfig config ->
    embed $
      let p   = port config
          app = createApp config
       in Warp.run p app
```

### putting everything together in a new main function

```haskell
main :: IO ()
main = do
  config <- loadConfig      -- loading the config as before
  serveAppFromConfig config -- declaring to run config based app on an AppServer
    & runWarpAppServer      -- use Warp to interprete this effect
    & runM                  -- finally lower the `Sem` stack into `IO ()`
```

Execution of the *inner* effect stack is now still handled by `createApp` but the actual control of the complete program now lies within the outer effect stack in the `main` function. The `inner` effect stack is run as part of the `runWarpAppServer` Polysemy interpreter function.


