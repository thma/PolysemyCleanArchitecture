module ExternalInterfaces.FileConfigProvider
(
    runFileConfigProvider
)
where

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

