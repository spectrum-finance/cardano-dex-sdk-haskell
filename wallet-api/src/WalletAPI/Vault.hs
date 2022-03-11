module WalletAPI.Vault where

import           RIO
import qualified Data.Set                   as Set

import           Ledger
import           Cardano.Api            hiding (Value)
import qualified Cardano.Api            as Crypto

import           CardanoTx.Models
import           WalletAPI.TrustStore
import           WalletAPI.UtxoStore
import qualified WalletAPI.Utxos     as U
import           Explorer.Service

data Vault f = Vault
  { getSigningKey       :: PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
  , selectUtxos         :: Value      -> f (Maybe (Set.Set FullTxOut))
  , selectUxtosByFilter :: (FullTxOut -> Bool) -> f (Maybe (Set.Set FullTxOut))
  }

narrowVault :: Vault f -> U.WalletOutputs f
narrowVault Vault{..} = U.WalletOutputs selectUtxos selectUxtosByFilter

mkVault :: MonadIO f => Explorer f -> TrustStore f -> KeyPass -> f (Vault f)
mkVault explorer tstore pass = do
  ustore <- mkUtxoStore
  pure $ Vault
    { getSigningKey = getSigningKey' tstore pass
    , selectUtxos   = selectUtxos' explorer ustore tstore
    , selectUxtosByFilter = selectUxtosByFilter' explorer ustore tstore
    }

getSigningKey' :: Functor f => TrustStore f -> KeyPass -> PubKeyHash -> f (Maybe ShelleyWitnessSigningKey)
getSigningKey' TrustStore{readSK} pass _ = readSK pass <&> WitnessPaymentKey <&> Just

selectUtxos' :: MonadIO f => Explorer f -> UtxoStore f -> TrustStore f -> Value -> f (Maybe (Set.Set FullTxOut))
selectUtxos' explorer ustore TrustStore{readVK} requiredValue =
  readVK <&> Crypto.verificationKeyHash >>= (\pkh -> U.selectUtxos'' explorer ustore pkh requiredValue)

selectUxtosByFilter' :: MonadIO f => Explorer f -> UtxoStore f -> TrustStore f -> (FullTxOut -> Bool) -> f (Maybe (Set.Set FullTxOut))
selectUxtosByFilter' explorer ustore TrustStore{readVK} predicate =
  readVK <&> Crypto.verificationKeyHash >>= (\pkh -> U.selectUxtosByFilter'' explorer ustore pkh predicate)
