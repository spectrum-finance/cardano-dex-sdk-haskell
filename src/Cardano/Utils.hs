module Cardano.Utils where

import           Ledger.Value
import           Ledger.Ada
import qualified PlutusTx.AssocMap as Map

import ErgoDex.Contracts.Types

lovelaceSubtract :: Value -> Ada -> Value
lovelaceSubtract vl Lovelace{getLovelace=v} =
    vl <> negValue
  where
    negValue = Value $ Map.fromList [(adaSymbol, Map.fromList [(adaToken, negate v)])]

filterValue :: Value -> AssetClass -> Value
filterValue (Value tokens) (AssetClass (cs, _)) = Value $ Map.delete cs tokens

coinAmountValue :: Coin a -> Amount a -> Value
coinAmountValue (Coin ac) (Amount v) = assetClassValue ac v
