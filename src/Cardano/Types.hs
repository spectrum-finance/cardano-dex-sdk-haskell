module Cardano.Types where

import Playground.Contract (FromJSON, ToJSON, Generic)

newtype Gix = Gix { unGix :: Integer } deriving (Show, Eq, Generic, FromJSON, ToJSON)
