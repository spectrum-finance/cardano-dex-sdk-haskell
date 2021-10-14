module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import Control.Monad (when)

import Ledger
import Ledger.Tx
import Ledger.Scripts (unitRedeemer)
import PlutusTx (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool as P
import           Cardano.Models

data OrderExecErr = PriceTooHigh deriving (Show)

data PoolActions = PoolActions
  { runSwap    :: Confirmed Swap    -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: Confirmed Deposit -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: Confirmed Redeem  -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  }

mkPoolActions :: PoolActions
mkPoolActions = undefined

runSwap' :: Confirmed Swap -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' (Confirmed swapOut Swap{..}) (Confirmed poolOut pool@Pool{..}) = do
  let
    poolIn  = FullTxIn poolOut Pay2Script (Just $ Redeemer $ toBuiltinData P.Swap)
    orderIn = FullTxIn swapOut Pay2Script (Just $ unitRedeemer)
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut pool') = swap pool (AssetAmount swapBase swapBaseIn)

    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

    rewardOut = TxOutCandidate
      { txOutCandidateAddress = pubKeyHashAddress swapRewardPkh
      , txOutCandidateValue   = assetAmountValue quoteOutput
      , txOutCandidateDatum   = Nothing
      }
    outputs = [nextPoolOut, rewardOut] -- todo: ex fee

  when (getAmount quoteOutput < swapMinQuoteOut) (Left PriceTooHigh)

  Right (TxCandidate inputs outputs, pp)
