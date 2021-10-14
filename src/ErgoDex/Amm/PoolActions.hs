module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import Control.Monad  (when)
import Data.Bifunctor

import           Ledger         (PubKeyHash(..), Redeemer(..), pubKeyHashAddress)
import qualified Ledger.Ada     as Ada
import           Ledger.Scripts (unitRedeemer)
import           PlutusTx       (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.Contracts.Types
import           Cardano.Models
import           Cardano.Utils

data OrderExecErr = PriceTooHigh deriving (Show)

data PoolActions = PoolActions
  { runSwap    :: Confirmed Swap    -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: Confirmed Deposit -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: Confirmed Redeem  -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  }

mkPoolActions :: PoolActions
mkPoolActions = undefined

runDeposit' :: PubKeyHash -> Confirmed Deposit -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runDeposit' executorPkh (Confirmed depositOut Deposit{..}) (Confirmed poolOut pool@Pool{..}) = do
  let
    poolIn  = FullTxIn poolOut Pay2Script (Just $ Redeemer $ toBuiltinData P.Deposit)
    orderIn = FullTxIn depositOut Pay2Script (Just unitRedeemer)
    inputs  = [poolIn, orderIn]

    (inX, inY) =
        bimap entryAmount entryAmount $
          if (assetEntryClass $ fst depositPair) == (unCoin poolCoinX)
          then depositPair
          else (snd depositPair, fst depositPair)
      where
        entryAmount (AssetEntry (_, v)) = Amount v

    pp@(Predicted nextPoolOut pool') = applyDeposit pool (inX, inY)

    executorFee = unExFee depositExFee
    executorOut = TxOutCandidate
      { txOutCandidateAddress = pubKeyHashAddress executorPkh
      , txOutCandidateValue   = Ada.lovelaceValueOf $ unAmount executorFee
      , txOutCandidateDatum   = Nothing
      }

    rewardOut =
      TxOutCandidate
        { txOutCandidateAddress = pubKeyHashAddress depositRewardPkh
        , txOutCandidateValue   = rewardValue
        , txOutCandidateDatum   = Nothing
        }
      where
        lqOutput        = liquidityAmount pool (inX, inY)
        initValue       = fullTxOutValue depositOut
        valueWithoutFee = lovelaceSubtract initValue (Ada.Lovelace $ unAmount executorFee)
        rewardValue     = (assetAmountValue lqOutput) <> valueWithoutFee

    outputs = [nextPoolOut, rewardOut, executorOut]

  Right (TxCandidate inputs outputs, pp)

runSwap' :: PubKeyHash -> Confirmed Swap -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' executorPkh (Confirmed swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (Confirmed poolOut pool@Pool{..}) = do
  let
    poolIn  = FullTxIn poolOut Pay2Script (Just $ Redeemer $ toBuiltinData P.Swap)
    orderIn = FullTxIn swapOut Pay2Script (Just unitRedeemer)
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut pool') = applySwap pool (AssetAmount swapBase swapBaseIn)

    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

    executorFee = (assetAmountRawValue quoteOutput) * exFeePerTokenNum `div` exFeePerTokenDen
    executorOut = TxOutCandidate
      { txOutCandidateAddress = pubKeyHashAddress executorPkh
      , txOutCandidateValue   = Ada.lovelaceValueOf executorFee
      , txOutCandidateDatum   = Nothing
      }

    rewardOut =
      TxOutCandidate
        { txOutCandidateAddress = pubKeyHashAddress swapRewardPkh
        , txOutCandidateValue   = rewardValue
        , txOutCandidateDatum   = Nothing
        }
      where
        initValue   = fullTxOutValue swapOut
        rewardValue = (assetAmountValue quoteOutput) <> (lovelaceSubtract initValue (Ada.Lovelace executorFee))

    outputs = [nextPoolOut, rewardOut, executorOut]

  when (getAmount quoteOutput < swapMinQuoteOut) (Left PriceTooHigh)

  Right (TxCandidate inputs outputs, pp)
