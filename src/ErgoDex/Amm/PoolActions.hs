module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import           Ledger.Tx
import           Ledger.Scripts (unitRedeemer)
import qualified PlutusTx

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool as P
import           Cardano.Models

data OrderExecErr

data PoolActions = PoolActions
  { runSwap    :: Confirmed Swap    -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: Confirmed Deposit -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: Confirmed Redeem  -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  }

mkPoolActions :: PoolActions
mkPoolActions = undefined

runSwap' :: Confirmed Swap -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' (Confirmed swapOut Swap{..}) (Confirmed poolOut Pool{..}) = undefined
--   let
--     orderIn = FullTxIn swapOutput Pay2Script (Just $ Redeemer $ unitRedeemer)
--     poolIn  = FullTxIn poolOutput Pay2Script (Just $ Redeemer $ PlutusTx.toData P.Swap)
--     inputs  = [orderIn, poolIn]
--     pool'   = swap pool (AssetAmount swapBase swapBaseIn) -- calculate next pool box along the way


