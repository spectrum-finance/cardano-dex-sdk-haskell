module ErgoDex.State where

import Cardano.Models
import Playground.Contract (FromJSON, ToJSON, Generic)

-- Predicted state of an on-chain entity `a`
data Predicted a = Predicted TxOutCandidate a
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Confirmed state of an on-chain entity `a`
data Confirmed a = Confirmed FullTxOut a
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
