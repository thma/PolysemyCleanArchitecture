{-# LANGUAGE TemplateHaskell #-}

module ExternalInterfaces.ConfigProvider where

import InterfaceAdapters.Config
import Polysemy

-- | The ConfigProvider effect can be used to provide an application with a Config instance.
data ConfigProvider m a where
  GetConfig :: ConfigProvider m Config

-- makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
-- getConfig :: Member ConfigProvider r => Sem r Config
makeSem ''ConfigProvider

