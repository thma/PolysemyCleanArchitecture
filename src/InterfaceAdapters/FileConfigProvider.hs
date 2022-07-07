module InterfaceAdapters.FileConfigProvider
(
    runFileConfigProvider
)
where

import InterfaceAdapters.Config
import InterfaceAdapters.ConfigProvider
import Polysemy (Embed, Member, Sem, embed, interpret)

-- | provides a Config object from a local file "application.config"
runFileConfigProvider :: (Member (Embed IO) r) => Sem (ConfigProvider : r) a -> Sem r a
runFileConfigProvider = interpret $ \case
  GetConfig -> embed loadConfig


-- | load application config from file "application.config"
loadConfig :: IO Config
loadConfig = do
  input <- readFile "application.config"
  pure $ read input

