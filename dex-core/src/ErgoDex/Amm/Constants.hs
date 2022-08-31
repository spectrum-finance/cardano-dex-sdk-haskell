module ErgoDex.Amm.Constants where

import Ledger.Ada              (lovelaceValueOf)
import Ledger.Value            (Value)
import ErgoDex.Contracts.Types (Amount(..), Lovelace)

minSafeOutputAmount :: Amount Lovelace
minSafeOutputAmount = Amount 3000000

minSafeOutputValue:: Value
minSafeOutputValue = lovelaceValueOf $ unAmount minSafeOutputAmount
