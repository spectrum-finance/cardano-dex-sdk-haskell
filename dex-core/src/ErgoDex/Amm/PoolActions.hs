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
import           ErgoDex.OffChain
import           ErgoDex.Amm.Scripts
import           ErgoDex.Utils
import           CardanoTx.Models

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
    executorFee = (assetAmountRawValue quoteOutput) * exFeePerTokenNum `div` exFeePerTokenDen
    executorOut = TxOutCandidate
      { txOutCandidateAddress  = pubKeyHashAddress executorPkh
      , txOutCandidateValue    = Ada.lovelaceValueOf executorFee
      , txOutCandidateDatum    = Nothing
      }

    rewardAddr = pubKeyHashAddress swapRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress  = rewardAddr
          , txOutCandidateValue    = rewardValue
          , txOutCandidateDatum    = Nothing
          }
      where
        initValue     = fullTxOutValue swapOut
        residualValue = excludeAda coinsResidue
          where coinsResidue = initValue <> (assetClassValue (unCoin swapBase) (negate $ unAmount swapBaseIn)) -- Remove Base input

        rewardValue = (assetAmountValue quoteOutput) <> residualValue

    outputs = [nextPoolOut, rewardOut, executorOut]

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = outputs
      , txCandidateValueMint    = MintValue mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo rewardAddr
      , txCandidateValidRange   = Interval.always
      }

  Right $ (txCandidate, pp)

runDeposit' :: PubKeyHash -> Confirmed Deposit -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runDeposit' executorPkh (Confirmed depositOut Deposit{..}) (poolOut, pool@Pool{..}) = do
  when (depositPoolId /= poolId) (Left $ PoolMismatch depositPoolId poolId)
  let
    poolIn  = mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Deposit)
    orderIn = mkScriptTxIn depositOut depositScript unitRedeemer
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
      }

    mintLqValue = assetAmountValue lqOutput
      where lqOutput = liquidityAmount pool (inX, inY)

    rewardAddr = pubKeyHashAddress depositRewardPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = rewardValue
          , txOutCandidateDatum     = Nothing
          }
      where
        initValue     = fullTxOutValue depositOut
        residualValue =
            excludeAda coinsResidue
          where
            coinsResidue =
                 initValue
              <> (assetClassValue (unCoin poolCoinX) (negate $ unAmount inX)) -- Remove X input
              <> (assetClassValue (unCoin poolCoinY) (negate $ unAmount inY)) -- Remove Y input
        rewardValue = residualValue <> mintLqValue

    outputs  = [nextPoolOut, rewardOut, executorOut]
    coinNft  = unPoolId poolId
    redeemer = Redeemer $ toBuiltinData coinNft
    mp       = liquidityMintingPolicyInstance coinNft
    mps      = mkMintInputs [(mp, redeemer)]

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = outputs
      , txCandidateValueMint    = MintValue mintLqValue
      , txCandidateMintInputs   = mps
      , txCandidateChangePolicy = Just $ ReturnTo rewardAddr
      , txCandidateValidRange   = Interval.always
      }

  Right $ (txCandidate, pp)

runRedeem' :: PubKeyHash -> Confirmed Redeem -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
runRedeem' executorPkh (Confirmed redeemOut Redeem{..}) (poolOut, pool@Pool{..}) = do
  when (redeemPoolId /= poolId) (Left $ PoolMismatch redeemPoolId poolId)
  let
    poolIn  = mkScriptTxIn poolOut poolScript (Redeemer $ toBuiltinData P.Redeem)
    orderIn = mkScriptTxIn redeemOut redeemScript unitRedeemer
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applyRedeem pool redeemLqIn

    executorFee = unExFee redeemExFee
    executorOut = TxOutCandidate
      { txOutCandidateAddress  = pubKeyHashAddress executorPkh
      , txOutCandidateValue    = Ada.lovelaceValueOf $ unAmount executorFee
      , txOutCandidateDatum    = Nothing
      }

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
        initValue     = fullTxOutValue redeemOut
        residualValue = excludeAda coinsResidue
          where coinsResidue = initValue <> burnLqValue -- Remove LQ input

        rewardValue = (assetAmountValue outX) <> (assetAmountValue outY) <> residualValue

    outputs  = [nextPoolOut, rewardOut, executorOut]
    coinNft  = unPoolId poolId
    redeemer = Redeemer $ toBuiltinData coinNft
    mp       = liquidityMintingPolicyInstance coinNft
    mps      = mkMintInputs [(mp, redeemer)]

    txCandidate = TxCandidate
      { txCandidateInputs       = Set.fromList inputs
      , txCandidateOutputs      = outputs
      , txCandidateValueMint    = MintValue burnLqValue
      , txCandidateMintInputs   = mps
      , txCandidateChangePolicy = Just $ ReturnTo rewardAddr
      , txCandidateValidRange   = Interval.always
      }

  Right $ (txCandidate, pp)
