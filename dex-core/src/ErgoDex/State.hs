module ErgoDex.State where

import Data.Aeson       (FromJSON, ToJSON)
import GHC.Generics     (Generic)
import CardanoTx.Models

-- An on-chain entity `a`
data OnChain a = OnChain FullTxOut a
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Predicted state of an on-chain entity `a`
data Predicted a = Predicted TxOutCandidate a
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- Confirmed state of an on-chain entity `a`
data Confirmed a = Confirmed FullTxOut a
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
