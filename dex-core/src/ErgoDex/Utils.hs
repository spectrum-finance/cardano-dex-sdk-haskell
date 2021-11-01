module ErgoDex.Utils where

import           Ledger.Value
import           Ledger.Ada        as Ada
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Prelude  as P

import ErgoDex.Contracts.Types

excludeAda :: Value -> Value
excludeAda (Value vs) = Value $ P.fmap (\ts -> Map.delete Ada.adaToken ts) vs

coinAmountValue :: Coin a -> Amount a -> Value
coinAmountValue (Coin ac) (Amount v) = assetClassValue ac v
