module CardanoTx.Value where

import qualified Data.Map as HMap
import qualified PlutusTx.AssocMap        as Map
import Plutus.V1.Ledger.Value 
  ( Value(Value), flattenValue )

unionVal :: Value -> Value -> Value
unionVal (Value l) (Value r) =
  let
    all2list = (fmap (\(a, b) -> (a, Map.toList b)) ( (Map.toList l) ++ (Map.toList r)))
    commonMap = HMap.fromListWith (++) all2list
  in Value $ Map.fromList $ fmap (\(a, b) -> (a, Map.fromList b)) (HMap.toList commonMap)

containsOnlyAda :: Value -> Bool
containsOnlyAda value = (length . flattenValue $ value) == 1