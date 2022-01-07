module ErgoDex.Utils where

import           Ledger.Value
import           Ledger.Ada        as Ada
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Prelude  as P

import ErgoDex.Contracts.Types

excludeAda :: Value -> Value
excludeAda (Value vs) = Value $ P.fmap (\ts -> Map.delete Ada.adaToken ts) vs

constantOneAdaValue :: Value
constantOneAdaValue =
  Value $ Map.singleton Ada.adaSymbol (Map.singleton Ada.adaToken 1000000)