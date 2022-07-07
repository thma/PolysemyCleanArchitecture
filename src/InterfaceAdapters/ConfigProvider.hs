{-# LANGUAGE TemplateHaskell #-}

module InterfaceAdapters.ConfigProvider where

import InterfaceAdapters.Config
import Polysemy

-- | The ConfigProvider effect can be used to provide and application with a Config instance.
data ConfigProvider m a where
  GetConfig :: ConfigProvider m Config

makeSem ''ConfigProvider

