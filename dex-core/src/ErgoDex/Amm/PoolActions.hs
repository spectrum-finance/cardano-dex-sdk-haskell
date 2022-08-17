module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr(..)
  , mkPoolActions
  ) where

import           Control.Exception.Base
import qualified Data.Set                   as Set
import           Data.Bifunctor
import           Data.Tuple
import           Control.Monad.Trans.Either (hoistEither, runEitherT)
import           RIO

import           Ledger          (Redeemer(..), PaymentPubKeyHash(..), pubKeyHashAddress)
import qualified Ledger.Interval as Interval
import qualified Ledger.Ada      as Ada
import           Ledger.Value    (assetClassValue)
import           PlutusTx        (toBuiltinData)
import           Ledger.Scripts  (Validator)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import qualified ErgoDex.Contracts.Pool        as P
import qualified ErgoDex.Contracts.Proxy.Order as O
import           ErgoDex.Contracts.Types
import           ErgoDex.PValidators
import           CardanoTx.Models

data OrderExecErr
  = PriceTooHigh
  | PoolMismatch PoolId PoolId
  | EmptyPool PoolId
  | PoolNotFoundInFinalTx PoolId
  deriving (Show)

instance Exception OrderExecErr

data PoolActions m = PoolActions
  { runSwap    :: Confirmed Swap    -> (FullTxOut, Pool) -> m (Either OrderExecErr (TxCandidate, Predicted Pool))
  , runDeposit :: Confirmed Deposit -> (FullTxOut, Pool) -> m (Either OrderExecErr (TxCandidate, Predicted Pool))
  , runRedeem  :: Confirmed Redeem  -> (FullTxOut, Pool) -> m (Either OrderExecErr (TxCandidate, Predicted Pool))
  }

mkPoolActions :: (MonadIO m) => PaymentPubKeyHash -> PoolActions m
mkPoolActions executorPkh = PoolActions
  { runSwap    = runSwap' executorPkh
  , runDeposit = runDeposit' executorPkh
  , runRedeem  = runRedeem' executorPkh
  }

newtype PoolIn  = PoolIn FullTxOut
newtype OrderIn = OrderIn FullTxOut

mkOrderInputs :: (MonadIO m) => P.PoolAction -> Validator -> PoolIn -> OrderIn -> m (Set.Set FullTxIn)
mkOrderInputs action orderValidator (PoolIn poolOut) (OrderIn orderOut) = do
    pool  <- poolValidator
    let
      preInputs = Set.fromList [poolOut, orderOut]
      poolIx    = toInteger $ Set.findIndex poolOut preInputs
      orderIx   = toInteger $ Set.findIndex orderOut preInputs
      poolIn    = mkScriptTxIn poolOut pool (Redeemer $ toBuiltinData $ P.PoolRedeemer action poolIx)
      orderIn   = mkScriptTxIn orderOut orderValidator (Redeemer $ toBuiltinData $ O.OrderRedeemer poolIx orderIx 1 O.Apply)
    pure $ Set.fromList [poolIn, orderIn]    

runSwap' :: (MonadIO m) => PaymentPubKeyHash -> Confirmed Swap -> (FullTxOut, Pool) -> m (Either OrderExecErr (TxCandidate, Predicted Pool))
runSwap' executorPkh (Confirmed swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (poolOut, pool) = runEitherT $ do
  swapValidator' <- lift swapValidator
  inputs <- lift $ mkOrderInputs P.Swap swapValidator' (PoolIn poolOut) (OrderIn swapOut)
  pp@(Predicted nextPoolOut _) <- lift $ applySwap pool (AssetAmount swapBase swapBaseIn)

  let 
    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

  when (swapPoolId /= poolId pool)               (hoistEither $ Left $ PoolMismatch swapPoolId (poolId pool))
  when (getAmount quoteOutput < swapMinQuoteOut) (hoistEither $ Left PriceTooHigh)

  let
    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash swapRewardPkh) swapRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress = rewardAddr
          , txOutCandidateValue   = rewardValue
          , txOutCandidateDatum   = EmptyDatum
          }
      where
        initValue     = fullTxOutValue swapOut
        exFee         = assetAmountRawValue quoteOutput * exFeePerTokenNum `div` exFeePerTokenDen
        residualValue =
             initValue
          <> assetClassValue (unCoin swapBase) (negate $ unAmount swapBaseIn) -- Remove Base input
          <> Ada.lovelaceValueOf (negate exFee)                               -- Remove Fee

        rewardValue = assetAmountValue quoteOutput <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  hoistEither $ Right (txCandidate, pp)

runDeposit' :: (MonadIO m) => PaymentPubKeyHash -> Confirmed Deposit -> (FullTxOut, Pool) -> m (Either OrderExecErr (TxCandidate, Predicted Pool))
runDeposit' executorPkh (Confirmed depositOut Deposit{..}) (poolOut, pool@Pool{..}) = runEitherT $ do
  when (depositPoolId /= poolId) (hoistEither $ Left $ PoolMismatch depositPoolId poolId)
  deposit <- lift depositValidator
  inputs  <- lift $ mkOrderInputs P.Deposit deposit (PoolIn poolOut) (OrderIn depositOut)
  let
    (inX, inY) =
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

    (unlockedLq, (Amount changeX, Amount changeY)) = rewardLp pool (netInX, netInY)

    alignmentValue =
         assetClassValue (unCoin poolCoinY) changeY
      <> assetClassValue (unCoin poolCoinX) changeX

  pp@(Predicted nextPoolOut _) <- lift $ applyDeposit pool (netInX, netInY)

  let
    mintLqValue = assetAmountValue (AssetAmount poolCoinLq unlockedLq)

    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash depositRewardPkh) depositRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress = rewardAddr
          , txOutCandidateValue   = rewardValue
          , txOutCandidateDatum   = EmptyDatum
          }
      where
        initValue     = fullTxOutValue depositOut
        residualValue =
             initValue
          <> assetClassValue (unCoin poolCoinX) (negate $ unAmount netInX) -- Remove X net input
          <> assetClassValue (unCoin poolCoinY) (negate $ unAmount netInY) -- Remove Y net input
          <> Ada.lovelaceValueOf (negate $ unAmount exFee)                 -- Remove Fee
        rewardValue = residualValue <> mintLqValue <> alignmentValue

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  hoistEither $ Right (txCandidate, pp)

runRedeem' :: (MonadIO m) => PaymentPubKeyHash -> Confirmed Redeem -> (FullTxOut, Pool) -> m (Either OrderExecErr (TxCandidate, Predicted Pool))
runRedeem' executorPkh (Confirmed redeemOut Redeem{..}) (poolOut, pool@Pool{..}) = runEitherT $ do
  when (redeemPoolId /= poolId) (hoistEither $ Left $ PoolMismatch redeemPoolId poolId)
  redeem <- redeemValidator
  inputs <- mkOrderInputs P.Redeem redeem (PoolIn poolOut) (OrderIn redeemOut)
  pp@(Predicted nextPoolOut _) <- lift $ applyRedeem pool redeemLqIn
  let
    burnLqValue = assetClassValue (unCoin redeemLq) (negate $ unAmount redeemLqIn)

    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash redeemRewardPkh) redeemRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress = rewardAddr
          , txOutCandidateValue   = rewardValue
          , txOutCandidateDatum   = EmptyDatum
          }
      where
        (outX, outY)  = sharesAmount pool redeemLqIn
        initValue     = fullTxOutValue redeemOut
        exFee         = Ada.lovelaceValueOf $ negate $ unAmount $ unExFee redeemExFee
        residualValue = initValue <> burnLqValue <> exFee -- Remove LQ input and ExFee

        rewardValue = assetAmountValue outX <> assetAmountValue outY <> residualValue

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  hoistEither $ Right (txCandidate, pp)
