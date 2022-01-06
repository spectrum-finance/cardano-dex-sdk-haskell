module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import           Control.Monad          (when)
import           Data.Bifunctor
import           Data.Tuple
import qualified Data.Set               as Set
import           Control.Exception.Base

import           Ledger          (PubKeyHash(..), Redeemer(..), pubKeyHashAddress)
import qualified Ledger.Interval as Interval
import qualified Ledger.Ada      as Ada
import           Ledger.Scripts  (unitRedeemer)
import           Ledger.Value    (assetClassValue)
import           PlutusTx        (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.Contracts.Types
import           ErgoDex.Amm.Scripts
import           CardanoTx.Models
import ErgoDex.Contracts.Proxy.Order (isAda)

data OrderExecErr =
    PriceTooHigh
  | PoolMismatch PoolId PoolId
  deriving (Show)

instance Exception OrderExecErr

data PoolActions = PoolActions
  { runSwap    :: Confirmed Swap    -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: Confirmed Deposit -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: Confirmed Redeem  -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  }

mkPoolActions :: PubKeyHash -> PoolActions
mkPoolActions executorPkh = PoolActions
  { runSwap    = runSwap' executorPkh
  , runDeposit = runDeposit' executorPkh
  , runRedeem  = runRedeem' executorPkh
  }

runSwap' :: PubKeyHash -> Confirmed Swap -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' executorPkh (Confirmed swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (poolOut, pool) = do
  let
    poolIn  = mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Swap)
    orderIn = mkScriptTxIn swapOut swapScript unitRedeemer
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applySwap pool (AssetAmount swapBase swapBaseIn)

    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

  when (swapPoolId /= poolId pool)               (Left $ PoolMismatch swapPoolId (poolId pool))
  when (getAmount quoteOutput < swapMinQuoteOut) (Left PriceTooHigh)

  let
    rewardAddr = pubKeyHashAddress swapRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress  = rewardAddr
          , txOutCandidateValue    = rewardValue
          , txOutCandidateDatum    = Nothing
          }
      where
        initValue     = fullTxOutValue swapOut
        exFee         = assetAmountRawValue quoteOutput * exFeePerTokenNum `div` exFeePerTokenDen
        residualValue =
             initValue
          <> assetClassValue (unCoin swapBase) (negate $ unAmount swapBaseIn) -- Remove Base input
          <> Ada.lovelaceValueOf exFee                                        -- Remove Fee

        rewardValue = assetAmountValue quoteOutput <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh
      , txCandidateValidRange   = Interval.always
      }

  Right (txCandidate, pp)

runDeposit' :: PubKeyHash -> Confirmed Deposit -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runDeposit' executorPkh (Confirmed depositOut Deposit{..}) (poolOut, pool@Pool{..}) = do
  when (depositPoolId /= poolId) (Left $ PoolMismatch depositPoolId poolId)
  let
    poolIn  = mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Deposit)
    orderIn = mkScriptTxIn depositOut depositScript unitRedeemer
    inputs  = [poolIn, orderIn]

    (inX :: Amount X, inY :: Amount Y) =
        bimap entryAmount entryAmount $
          if assetEntryClass (fst depositPair) == unCoin poolCoinX
          then depositPair
          else swap depositPair
      where entryAmount (AssetEntry (_, v)) = Amount v

    exFee = unExFee depositExFee

    (netInX, netInY)
      | isAda poolCoinX = (inX - retagAmount exFee - retagAmount adaCollateral, inY)
      | isAda poolCoinY = (inX, inY - retagAmount exFee - retagAmount adaCollateral)
      | otherwise       = (inX, inY)

    pp@(Predicted nextPoolOut _) = applyDeposit pool (netInX, netInY)

    mintLqValue = assetAmountValue lqOutput
      where lqOutput = liquidityAmount pool (netInX, netInY)

    rewardAddr = pubKeyHashAddress depositRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress = rewardAddr
          , txOutCandidateValue   = rewardValue
          , txOutCandidateDatum   = Nothing
          }
      where
        initValue     = fullTxOutValue depositOut
        residualValue =
             initValue
          <> assetClassValue (unCoin poolCoinX) (negate $ unAmount netInX) -- Remove X net input
          <> assetClassValue (unCoin poolCoinY) (negate $ unAmount netInY) -- Remove Y net input
          <> Ada.lovelaceValueOf (negate $ unAmount exFee)                 -- Remove Fee
        rewardValue = residualValue <> mintLqValue

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh
      , txCandidateValidRange   = Interval.always
      }

  Right (txCandidate, pp)

runRedeem' :: PubKeyHash -> Confirmed Redeem -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runRedeem' executorPkh (Confirmed redeemIn Redeem{..}) (poolOut, pool@Pool{..}) = do
  when (redeemPoolId /= poolId) (Left $ PoolMismatch redeemPoolId poolId)
  let
    poolIn  = mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Redeem)
    orderIn = mkScriptTxIn redeemIn redeemScript unitRedeemer
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applyRedeem pool redeemLqIn

    burnLqValue = assetClassValue (unCoin redeemLq) (negate $ unAmount redeemLqIn)

    rewardAddr = pubKeyHashAddress redeemRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = rewardValue
          , txOutCandidateDatum     = Nothing
          }
      where
        (outX, outY)  = sharesAmount pool redeemLqIn
        initValue     = fullTxOutValue redeemIn
        exFee         = Ada.lovelaceValueOf $ negate $ unAmount $ unExFee redeemExFee
        residualValue = initValue <> burnLqValue <> exFee -- Remove LQ input and ExFee

        rewardValue = assetAmountValue outX <> assetAmountValue outY <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh
      , txCandidateValidRange   = Interval.always
      }

  Right (txCandidate, pp)
