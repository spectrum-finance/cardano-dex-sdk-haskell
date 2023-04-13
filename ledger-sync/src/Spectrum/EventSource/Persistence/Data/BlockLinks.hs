module Spectrum.EventSource.Persistence.Data.BlockLinks where

import GHC.Generics
  ( Generic )

import Data.Aeson
  ( ToJSON, FromJSON )

import Ledger
  ( TxId )
import Spectrum.EventSource.Types
  ( ConcretePoint )

data BlockLinks = BlockLinks
  { prevPoint  :: ConcretePoint
  , txIds      :: [TxId]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)
