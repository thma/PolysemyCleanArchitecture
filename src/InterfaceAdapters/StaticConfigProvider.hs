module InterfaceAdapters.StaticConfigProvider 
(
    runStaticConfigProvider
)
where

import InterfaceAdapters.Config
import InterfaceAdapters.ConfigProvider
import Polysemy (Embed, Member, Sem, embed, interpret, runM)


runStaticConfigProvider :: (Member (Embed IO) r) => Sem (ConfigProvider : r) a -> Sem r a
runStaticConfigProvider = interpret $ \case
  GetConfig confName -> embed loadConfig


-- | load application config. In real life, this would load a config file or read commandline args.
loadConfig :: IO Config
loadConfig = return Config {port = 8080, backend = SQLite, dbPath = "kvs.db", verbose = True}
