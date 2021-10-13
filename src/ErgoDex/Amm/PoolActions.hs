module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import           Ledger.Tx
import           Ledger.Scripts (unitRedeemer)
import qualified PlutusTx

import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool as P
import Cardano.Models

data OrderExecErr

data PoolActions = PoolActions
  { runSwap    :: OrderAction Swap    -> Pool -> Either OrderExecErr (TxCandidate, Pool)
  , runDeposit :: OrderAction Deposit -> Pool -> Either OrderExecErr (TxCandidate, Pool)
  , runRedeem  :: OrderAction Redeem  -> Pool -> Either OrderExecErr (TxCandidate, Pool)
  }

mkPoolActions :: PoolActions
mkPoolActions = undefined

runSwap' :: OrderAction Swap -> Pool -> Either OrderExecErr (TxCandidate, Pool)
runSwap' (SwapAction Swap{..}) Pool{..} = undefined
-- runSwap' (OrderAction Swap{..}) Pool{..} = do
--   let
--     orderIn = FullTxIn swapOutput Pay2Script (Just $ Redeemer $ unitRedeemer)
--     poolIn  = FullTxIn poolOutput Pay2Script (Just $ Redeemer $ PlutusTx.toData P.Swap)
--     inputs  = [orderIn, poolIn]

