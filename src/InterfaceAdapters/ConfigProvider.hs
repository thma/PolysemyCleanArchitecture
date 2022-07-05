{-# LANGUAGE TemplateHaskell #-}

module InterfaceAdapters.ConfigProvider where

import InterfaceAdapters.Config
import Polysemy (makeSem)

data ConfigProvider m a where
  GetConfig :: ConfigProvider m Config

makeSem ''ConfigProvider
