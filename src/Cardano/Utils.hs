module Cardano.Utils where

import           Plutus.V1.Ledger.Value
import           ErgoDex.Types     (AssetAmount(..), assetAmountCoinOf)
import           Ledger
import           Cardano.Models
import           Ledger.Value()
import           Ledger.Ada
import qualified PlutusTx.AssocMap as Map
import           ErgoDex.Contracts.Types

lovelaceSubtract :: Value -> Ada -> Value
lovelaceSubtract vl Lovelace{getLovelace=v} =
    vl <> negValue
  where
    negValue = Value $ Map.fromList [(adaSymbol, Map.fromList [(adaToken, negate v)])]

removeAssetClassFromValue :: AssetClass -> Value -> Value
removeAssetClassFromValue AssetClass{..} Value{..} =
    Value $ Map.delete curSymbol2Del getValue
  where
    curSymbol2Del = fst unAssetClass
