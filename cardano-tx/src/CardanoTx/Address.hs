module CardanoTx.Address
  ( readShellyAddress
  ) where

import Data.Text
import Data.Either.Combinators

import qualified Ledger                     as P
import qualified Cardano.Api                as C
import qualified Ledger.Tx.CardanoAPI       as Interop

readShellyAddress :: Text -> Maybe P.Address
readShellyAddress text = do
  saddr <- C.deserialiseAddress (C.AsAddress C.AsShelleyAddr) text
  rightToMaybe $ Interop.fromCardanoAddressInEra (C.shelleyAddressInEra saddr :: C.AddressInEra C.BabbageEra)
