module WalletAPI.Vault where

import           RIO
import qualified Data.Set as Set

import Ledger
import Cardano.Api hiding (Value)

import CardanoTx.Models
import WalletAPI.TrustStore
import WalletAPI.UtxoStore
import Explorer.Service

data Vault f = Vault
  { getSigningKey :: PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
  , selectUtxos   :: Value      -> f (Maybe (Set.Set FullTxOut))
  }

mkVault :: Explorer f -> TrustStore f -> Vault f
mkVault Explorer{..} TrustStore{..} = undefined

getSigningKey' :: Functor f => TrustStore f -> KeyPass -> PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
getSigningKey' TrustStore{..} pass _ = readSK pass <&> WitnessPaymentKey <&> Just

selectUtxos' :: Monad f => Explorer f -> TrustStore f -> Value -> f (Maybe (Set.Set FullTxOut))
selectUtxos' Explorer{..} TrustStore{..} value = undefined
