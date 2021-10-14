module Cardano.Utils where

import           Ledger.Value
import           Ledger.Ada
import qualified PlutusTx.AssocMap as Map

lovelaceSubtract :: Value -> Ada -> Value
lovelaceSubtract vl Lovelace{getLovelace=v} =
    vl <> negValue
  where
    negValue = Value $ Map.fromList [(adaSymbol, Map.fromList [(adaToken, negate v)])]
