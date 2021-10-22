module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import Control.Monad  (when)
import Data.Bifunctor
import Data.Tuple

import           Ledger         (PubKeyHash(..), Redeemer(..), pubKeyHashAddress)
import qualified Ledger.Ada     as Ada
import           Ledger.Scripts (unitRedeemer)
import           Ledger.Value   (assetClassValue)
import           PlutusTx       (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.Contracts.Types
import           ErgoDex.Amm.Scripts
import           Cardano.Models
import           Cardano.Utils

data OrderExecErr =
    PriceTooHigh
  | PoolMismatch PoolId PoolId
  deriving (Show)

data PoolActions = PoolActions
  { runSwap    :: Confirmed Swap    -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: Confirmed Deposit -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: Confirmed Redeem  -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  }

mkPoolActions :: PubKeyHash -> PoolActions
mkPoolActions executorPkh = PoolActions
  { runSwap    = runSwap' executorPkh
  , runDeposit = runDeposit' executorPkh
  , runRedeem  = runRedeem' executorPkh
  }

runSwap' :: PubKeyHash -> Confirmed Swap -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' executorPkh (Confirmed swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (Confirmed poolOut pool) = do
  let
    poolIn  = FullTxIn poolOut (Just poolScript) (Just $ Redeemer $ toBuiltinData P.Swap)
    orderIn = FullTxIn swapOut (Just swapScript) (Just unitRedeemer)
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applySwap pool (AssetAmount swapBase swapBaseIn)

    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

    executorFee = (assetAmountRawValue quoteOutput) * exFeePerTokenNum `div` exFeePerTokenDen
    executorOut = TxOutCandidate
      { txOutCandidateAddress  = pubKeyHashAddress executorPkh
      , txOutCandidateValue    = Ada.lovelaceValueOf executorFee
      , txOutCandidateDatum    = Nothing
      , txOutCandidatePolicies = []
      }

    rewardAddr = pubKeyHashAddress swapRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress  = rewardAddr
          , txOutCandidateValue    = rewardValue
          , txOutCandidateDatum    = Nothing
          , txOutCandidatePolicies = []
          }
      where
        initValue     = fullTxOutValue swapOut
        residualValue = excludeAda coinsResidue
          where coinsResidue = initValue <> (assetClassValue (unCoin swapBase) (negate $ unAmount swapBaseIn)) -- Remove Base input

        rewardValue = (assetAmountValue quoteOutput) <> residualValue

    outputs = [nextPoolOut, rewardOut, executorOut]

  when (swapPoolId /= poolId pool)               (Left $ PoolMismatch swapPoolId (poolId pool))
  when (getAmount quoteOutput < swapMinQuoteOut) (Left PriceTooHigh)

  Right (TxCandidate inputs outputs (Just $ ReturnTo rewardAddr), pp)

runDeposit' :: PubKeyHash -> Confirmed Deposit -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runDeposit' executorPkh (Confirmed depositOut Deposit{..}) (Confirmed poolOut pool@Pool{..}) = do
  let
    poolIn  = FullTxIn poolOut (Just poolScript) (Just $ Redeemer $ toBuiltinData P.Deposit)
    orderIn = FullTxIn depositOut (Just depositScript) (Just unitRedeemer)
    inputs  = [poolIn, orderIn]

    (inX, inY) =
        bimap entryAmount entryAmount $
          if (assetEntryClass $ fst depositPair) == (unCoin poolCoinX)
          then depositPair
          else swap depositPair
      where
        entryAmount (AssetEntry (_, v)) = Amount v

    pp@(Predicted nextPoolOut _) = applyDeposit pool (inX, inY)

    executorFee = unExFee depositExFee
    executorOut = TxOutCandidate
      { txOutCandidateAddress  = pubKeyHashAddress executorPkh
      , txOutCandidateValue    = Ada.lovelaceValueOf $ unAmount executorFee
      , txOutCandidateDatum    = Nothing
      , txOutCandidatePolicies = []
      }

    rewardAddr = pubKeyHashAddress depositRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress  = rewardAddr
          , txOutCandidateValue    = rewardValue
          , txOutCandidateDatum    = Nothing
          , txOutCandidatePolicies = []
          }
      where
        lqOutput      = liquidityAmount pool (inX, inY)
        initValue     = fullTxOutValue depositOut
        residualValue =
            excludeAda coinsResidue
          where
            coinsResidue =
                 initValue
              <> (assetClassValue (unCoin poolCoinX) (negate $ unAmount inX)) -- Remove X input
              <> (assetClassValue (unCoin poolCoinY) (negate $ unAmount inY)) -- Remove Y input

        rewardValue = (assetAmountValue lqOutput) <> residualValue

    outputs = [nextPoolOut, rewardOut, executorOut]

  when (depositPoolId /= poolId) (Left $ PoolMismatch depositPoolId poolId)

  Right (TxCandidate inputs outputs (Just $ ReturnTo rewardAddr), pp)

runRedeem' :: PubKeyHash -> Confirmed Redeem -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runRedeem' executorPkh (Confirmed redeemOut Redeem{..}) (Confirmed poolOut pool@Pool{..}) = do
  let
    poolIn  = FullTxIn poolOut (Just poolScript) (Just $ Redeemer $ toBuiltinData P.Redeem)
    orderIn = FullTxIn redeemOut (Just redeemScript) (Just unitRedeemer)
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applyRedeem pool redeemLqIn

    executorFee = unExFee redeemExFee
    executorOut = TxOutCandidate
      { txOutCandidateAddress  = pubKeyHashAddress executorPkh
      , txOutCandidateValue    = Ada.lovelaceValueOf $ unAmount executorFee
      , txOutCandidateDatum    = Nothing
      , txOutCandidatePolicies = []
      }

    rewardAddr = pubKeyHashAddress redeemRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress  = rewardAddr
          , txOutCandidateValue    = rewardValue
          , txOutCandidateDatum    = Nothing
          , txOutCandidatePolicies = []
          }
      where
        (outX, outY)  = sharesAmount pool redeemLqIn
        initValue     = fullTxOutValue redeemOut
        residualValue = excludeAda coinsResidue
          where coinsResidue = initValue <> (assetClassValue (unCoin redeemLq) (negate $ unAmount redeemLqIn)) -- Remove LQ input

        rewardValue = (assetAmountValue outX) <> (assetAmountValue outY) <> residualValue

    outputs = [nextPoolOut, rewardOut, executorOut]

  when (redeemPoolId /= poolId) (Left $ PoolMismatch redeemPoolId poolId)

  Right (TxCandidate inputs outputs (Just $ ReturnTo rewardAddr), pp)
