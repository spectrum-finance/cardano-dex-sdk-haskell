module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import Plutus.V1.Ledger.Tx

data OrderExecErr

data PoolActions = PoolActions
  { runSwap    :: OrderAction Swap    -> Pool -> Either OrderExecErr (Tx, Pool)
  , runDeposit :: OrderAction Deposit -> Pool -> Either OrderExecErr (Tx, Pool)
  , runRedeem  :: OrderAction Redeem  -> Pool -> Either OrderExecErr (Tx, Pool)
  }

mkPoolActions :: PoolActions
mkPoolActions = undefined
