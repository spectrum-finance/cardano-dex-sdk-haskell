module ErgoDex.Amm.Constants where

import Plutus.V1.Ledger.Ada    (lovelaceValueOf)
import Ledger.Value
import ErgoDex.Contracts.Types

minSafeOutputAmount :: Amount Lovelace
minSafeOutputAmount = Amount 3000000

minSafeOutputValue:: Value
minSafeOutputValue = lovelaceValueOf $ unAmount minSafeOutputAmount
