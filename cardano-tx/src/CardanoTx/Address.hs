module CardanoTx.Address
  ( renderToShellyAddress
  , readShellyAddress
  ) where

import Data.Text
import Data.Either.Combinators

import qualified Ledger.Typed.Scripts     as Scripts
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Ledger.Scripts           as Scripts
import qualified Ledger                   as P
import qualified Cardano.Api              as C
import qualified Plutus.Contract.CardanoAPI as Interop

renderToShellyAddress :: C.NetworkId -> forall a . Scripts.TypedValidator a -> Text
renderToShellyAddress network validatorInstance =
    (C.serialiseAddress . C.makeShelleyAddress network paymentCredential) C.NoStakeAddress
  where
    validatorScript   = Scripts.unValidatorScript $ Scripts.validatorScript validatorInstance
    hashScript        = C.hashScript $ Scripts.toCardanoApiScript validatorScript
    paymentCredential = C.PaymentCredentialByScript hashScript

readShellyAddress :: Text -> Maybe P.Address
readShellyAddress text = do
  saddr <- C.deserialiseAddress (C.AsAddress C.AsShelleyAddr) text
  rightToMaybe $ Interop.fromCardanoAddress $ (C.shelleyAddressInEra saddr :: C.AddressInEra C.AlonzoEra)
