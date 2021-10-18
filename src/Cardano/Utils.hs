module Cardano.Utils where

import           Ledger.Value
import           Ledger.Ada
import qualified PlutusTx.AssocMap as Map

import ErgoDex.Contracts.Types

lovelaceSubtract :: Value -> Ada -> Value
lovelaceSubtract vl ada =
    vl <> negValue
  where
    negValue = toValue ada

filterValue :: Value -> AssetClass -> Value
filterValue (Value tokens) (AssetClass (cs, _)) = Value $ Map.delete cs tokens

coinAmountValue :: Coin a -> Amount a -> Value
coinAmountValue (Coin ac) (Amount v) = assetClassValue ac v
