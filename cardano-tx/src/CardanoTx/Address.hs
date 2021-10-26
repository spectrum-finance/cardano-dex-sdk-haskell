module CardanoTx.Address
  ( renderToShellyAddress
  ) where

import qualified Ledger.Typed.Scripts     as Scripts
import qualified Plutus.V1.Ledger.Scripts as PlutusScripts
import qualified Ledger.Scripts           as LedgerScripts
import qualified Cardano.Api              as Script
import           Data.Text

renderToShellyAddress :: Script.NetworkId -> forall a . Scripts.TypedValidator a -> Text
renderToShellyAddress network validatorInstance =
    (Script.serialiseAddress . Script.makeShelleyAddress network paymentCredential) Script.NoStakeAddress
  where
    validatorScript   = PlutusScripts.unValidatorScript $ Scripts.validatorScript validatorInstance
    hashScript        = Script.hashScript $ LedgerScripts.toCardanoApiScript validatorScript
    paymentCredential = Script.PaymentCredentialByScript hashScript