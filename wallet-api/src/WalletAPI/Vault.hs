module WalletAPI.Vault where

import qualified Data.Set as Set

import Ledger
import Cardano.Api hiding (Value)

import CardanoTx.Models

data Vault f = Vault
  { getSigningKey :: PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
  , getUtxos      :: Value -> f (Set.Set FullTxOut)
  }
