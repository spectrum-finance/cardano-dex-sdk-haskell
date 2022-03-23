module WalletAPI.Vault where

import RIO

import           Ledger      ( PubKeyHash )
import           Cardano.Api ( Hash, PaymentKey, ShelleyWitnessSigningKey(WitnessPaymentKey) )
import qualified Cardano.Api as Crypto

import WalletAPI.TrustStore ( TrustStore(TrustStore, readSK, readVK), KeyPass )

data Vault f = Vault
  { getSigningKey     :: PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
  , getPaymentKeyHash :: f (Hash PaymentKey)
  }

mkVault :: Functor f => TrustStore f Crypto.PaymentKey -> KeyPass -> Vault f
mkVault tstore pass = do
  Vault
    { getSigningKey     = getSigningKey' tstore pass
    , getPaymentKeyHash = getPaymentKeyHash' tstore
    }

getSigningKey' :: Functor f => TrustStore f Crypto.PaymentKey -> KeyPass -> PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
getSigningKey' TrustStore{readSK} pass _ = readSK pass <&> WitnessPaymentKey <&> Just

getPaymentKeyHash' :: Functor f => TrustStore f Crypto.PaymentKey -> f (Hash PaymentKey)
getPaymentKeyHash' TrustStore{readVK} = readVK <&> Crypto.verificationKeyHash
