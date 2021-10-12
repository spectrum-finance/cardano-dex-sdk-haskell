module ErgoDex.InterpreterService
    ( InterpreterService(..)
    , Err
    , mkInterpreterService
    ) where

import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import Plutus.V1.Ledger.Tx

data Err
data InterpreterService = InterpreterService
    { swap    :: OrderAction Swap    -> Pool -> Either Err Tx
    , deposit :: OrderAction Deposit -> Pool -> Either Err Tx
    , redeem  :: OrderAction Redeem  -> Pool -> Either Err Tx
    }

mkInterpreterService :: InterpreterService
mkInterpreterService = undefined