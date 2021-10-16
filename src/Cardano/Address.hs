module Cardano.Address
  ( mkScriptAddress
  ) where

import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as PlutusScripts
import qualified Ledger.Scripts as LedgerScripts
import qualified Cardano.Api as Script
import Data.Text

mkScriptAddress :: forall a . Scripts.TypedValidator a -> Text
mkScriptAddress validatorInstance =
    (Script.serialiseAddress . Script.makeShelleyAddress Script.Mainnet paymentCredential) Script.NoStakeAddress
    where
      validatorScript   = PlutusScripts.unValidatorScript $ Scripts.validatorScript validatorInstance
      hashScript        = Script.hashScript $ LedgerScripts.toCardanoApiScript validatorScript
      paymentCredential = Script.PaymentCredentialByScript hashScript