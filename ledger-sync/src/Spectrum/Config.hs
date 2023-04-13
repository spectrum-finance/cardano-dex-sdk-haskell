module Spectrum.Config where

import GHC.Generics
  ( Generic )
import Dhall
  ( FromDhall, input, auto )

import Spectrum.EventSource.Types
  ( ConcretePoint )

data EventSourceConfig = EventSourceConfig
  { startAt :: !ConcretePoint
  } deriving (Generic, FromDhall)