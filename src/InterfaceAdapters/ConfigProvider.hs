{-# LANGUAGE TemplateHaskell #-}

module InterfaceAdapters.ConfigProvider where

import InterfaceAdapters.Config
import Polysemy
import Polysemy.Internal

data ConfigProvider m a where
  GetConfig :: String -> ConfigProvider m Config

makeSem ''ConfigProvider

{--
getConfig :: Member ConfigProvider r => String -> Sem r Config
getConfig x = send (GetConfig x :: ConfigProvider (Sem r) Config)
--}
