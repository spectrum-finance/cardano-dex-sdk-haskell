module WalletAPI.Vault where

import qualified Data.Set as Set

import Ledger
import Cardano.Api hiding (Value)

import CardanoTx.Models

data Vault f = Vault
  { getSigningKey :: PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
  , selectUtxos   :: Value      -> f (Maybe (Set.Set FullTxOut))
  }
