module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr(..)
  , PoolActionsConfig(..)
  , mkPoolActions
  , AmmValidators(..)
  , fetchValidatorsV1
  ) where

import           Control.Exception.Base
import qualified Data.Set                   as Set
import           Dhall                      (FromDhall)
import           Data.Bifunctor
import           Data.Tuple
import           RIO


import           Ledger              (Redeemer(..), PaymentPubKeyHash(..), pubKeyHashAddress)
import qualified Ledger.Interval     as Interval
import qualified Ledger.Ada          as Ada
import           Ledger.Value        (assetClassValue)
import           PlutusTx            (toBuiltinData)

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import           ErgoDex.Validators
import qualified ErgoDex.Contracts.Pool        as P
import qualified ErgoDex.Contracts.Proxy.Order as O
import           ErgoDex.Contracts.Types
import           CardanoTx.Models
import Debug.Trace

data PoolActionsConfig = PoolActionsConfig
  { safeTxFeeLovalace :: Integer
  } deriving (Generic, FromDhall)

data OrderExecErr
  = PriceTooHigh
  | PoolMismatch PoolId PoolId
  | EmptyPool PoolId
  | PoolNotFoundInFinalTx PoolId
  deriving (Show)

instance Exception OrderExecErr

data AmmValidators ver = AmmValidators
  { poolV    :: PoolValidator ver
  , swapV    :: SwapValidator ver
  , depositV :: DepositValidator ver
  , redeemV  :: RedeemValidator ver
  }

fetchValidatorsV1 :: MonadIO m => m (AmmValidators V1)
fetchValidatorsV1 =
  AmmValidators
    <$> fetchPoolValidatorV1
    <*> fetchSwapValidatorV1
    <*> fetchDepositValidatorV1
    <*> fetchRedeemValidatorV1

data PoolActions = PoolActions
  { runSwap    :: [FullTxOut] -> OnChain Swap    -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: [FullTxOut] -> OnChain Deposit -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: [FullTxOut] -> OnChain Redeem  -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool)
  }

mkPoolActions :: PoolActionsConfig -> PaymentPubKeyHash -> AmmValidators V1 -> PoolActions
mkPoolActions cfg executorPkh AmmValidators{..} = PoolActions
  { runSwap    = runSwap' cfg executorPkh poolV swapV
  , runDeposit = runDeposit' cfg executorPkh poolV depositV
  , runRedeem  = runRedeem' cfg executorPkh poolV redeemV
  }

newtype PoolIn  = PoolIn FullTxOut
newtype OrderIn = OrderIn FullTxOut

mkOrderInputs
  :: forall kind. P.PoolAction
  -> PoolValidator V1
  -> OrderValidator kind V1
  -> PoolIn
  -> OrderIn
  -> Set.Set FullTxIn
mkOrderInputs action (PoolValidator pv) ov' (PoolIn poolOut) (OrderIn orderOut) =
    let
      ov        = orderValidator ov'
      preInputs = Set.fromList [poolOut, orderOut]
      poolIx    = toInteger $ Set.findIndex poolOut preInputs
      orderIx   = toInteger $ Set.findIndex orderOut preInputs
      poolIn    = mkScriptTxIn poolOut pv (Redeemer $ toBuiltinData $ P.PoolRedeemer action poolIx)
      orderIn   = mkScriptTxIn orderOut ov (Redeemer $ toBuiltinData $ O.OrderRedeemer poolIx orderIx 1 O.Apply)
    in Set.fromList [poolIn, orderIn]

runSwap'
  :: PoolActionsConfig
  -> PaymentPubKeyHash
  -> PoolValidator V1
  -> SwapValidator V1
  -> [FullTxOut]
  -> OnChain Swap
  -> (FullTxOut, Pool)
  -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' PoolActionsConfig{..} executorPkh pv sv refInputs (OnChain swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (poolOut, pool) = do
  let
    inputs = mkOrderInputs P.Swap pv sv (PoolIn poolOut) (OrderIn swapOut)
    pp@(Predicted nextPoolOut _) = applySwap pv pool (AssetAmount swapBase swapBaseIn)
    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

  when (swapPoolId /= poolId pool)               (Left $ PoolMismatch swapPoolId (poolId pool))
  when (getAmount quoteOutput < swapMinQuoteOut) (Left PriceTooHigh)

  let
    exFee      = assetAmountRawValue quoteOutput * exFeePerTokenNum `div` exFeePerTokenDen
    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash swapRewardPkh) swapRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = rewardValue
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }
      where
        initValue     = fullTxOutValue swapOut
        residualValue =
             initValue
          <> assetClassValue (unCoin swapBase) (negate $ unAmount swapBaseIn) -- Remove Base input
          <> Ada.lovelaceValueOf (negate exFee)                               -- Remove Batcher Fee

        rewardValue = assetAmountValue quoteOutput <> residualValue
  
    executorRewardPkh = pubKeyHashAddress executorPkh Nothing
    executorRewardOut = 
        TxOutCandidate
          { txOutCandidateAddress   = executorRewardPkh
          , txOutCandidateValue     = Ada.lovelaceValueOf (exFee - safeTxFeeLovalace)
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut, executorRewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just . ReturnTo $ rewardAddr
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Debug.Trace.traceM ("PoolIn " ++ show poolOut)
  Debug.Trace.traceM ("OrderIn " ++ show swapOut)
  Debug.Trace.traceM ("exFeePerTokenNum " ++ show exFeePerTokenNum)
  Debug.Trace.traceM ("exFeePerTokenDen " ++ show exFeePerTokenDen)
  Debug.Trace.traceM ("exFee " ++ show exFee)
  Debug.Trace.traceM ("quoteOutput " ++ show quoteOutput)
  Debug.Trace.traceM ("RewardOut" ++ show rewardOut)
  Debug.Trace.traceM ("executorRewardOut " ++ show executorRewardOut)

  Right (txCandidate, pp)

runDeposit'
  :: PoolActionsConfig
  -> PaymentPubKeyHash
  -> PoolValidator V1
  -> DepositValidator V1
  -> [FullTxOut]
  -> OnChain Deposit
  -> (FullTxOut, Pool)
  -> Either OrderExecErr (TxCandidate, Predicted Pool)
runDeposit' PoolActionsConfig{..} executorPkh pv dv refInputs (OnChain depositOut Deposit{..}) (poolOut, pool@Pool{..}) = do
  when (depositPoolId /= poolId) (Left $ PoolMismatch depositPoolId poolId)
  let
    inputs = mkOrderInputs P.Deposit pv dv (PoolIn poolOut) (OrderIn depositOut)

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

    pp@(Predicted nextPoolOut _) = applyDeposit pv pool (netInX, netInY)

    mintLqValue = assetAmountValue (AssetAmount poolCoinLq unlockedLq)

    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash depositRewardPkh) depositRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = rewardValue
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }
      where
        initValue     = fullTxOutValue depositOut
        residualValue =
             initValue
          <> assetClassValue (unCoin poolCoinX) (negate $ unAmount netInX) -- Remove X net input
          <> assetClassValue (unCoin poolCoinY) (negate $ unAmount netInY) -- Remove Y net input
          <> Ada.lovelaceValueOf (negate $ unAmount exFee)                 -- Remove Fee
        rewardValue = residualValue <> mintLqValue <> alignmentValue

    executorRewardPkh = pubKeyHashAddress executorPkh Nothing
    exexutorRewardOut = 
        TxOutCandidate
          { txOutCandidateAddress   = executorRewardPkh
          , txOutCandidateValue     = Ada.lovelaceValueOf $ (unAmount exFee - safeTxFeeLovalace)
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut, exexutorRewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just . ReturnTo $ rewardAddr
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp)

runRedeem'
  :: PoolActionsConfig
  -> PaymentPubKeyHash
  -> PoolValidator V1
  -> RedeemValidator V1
  -> [FullTxOut]
  -> OnChain Redeem
  -> (FullTxOut, Pool)
  -> Either OrderExecErr (TxCandidate, Predicted Pool)
runRedeem' PoolActionsConfig{..} executorPkh pv rv refInputs (OnChain redeemOut Redeem{..}) (poolOut, pool@Pool{..}) = do
  when (redeemPoolId /= poolId) (Left $ PoolMismatch redeemPoolId poolId)
  let
    inputs = mkOrderInputs P.Redeem pv rv (PoolIn poolOut) (OrderIn redeemOut)

    pp@(Predicted nextPoolOut _) = applyRedeem pv pool redeemLqIn

    burnLqValue = assetClassValue (unCoin redeemLq) (negate $ unAmount redeemLqIn)
    
    exFee = unAmount $ unExFee redeemExFee

    rewardAddr = pubKeyHashAddress (PaymentPubKeyHash redeemRewardPkh) redeemRewardSPkh
    rewardOut  =
        TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = rewardValue
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }
      where
        (outX, outY)  = sharesAmount pool redeemLqIn
        initValue     = fullTxOutValue redeemOut
        negatedExFe   = Ada.lovelaceValueOf . negate $ exFee
        residualValue = 
             initValue 
          <> burnLqValue 
          <> negatedExFe                                     -- Remove LQ input and ExFee

        rewardValue = assetAmountValue outX <> assetAmountValue outY <> residualValue
    
    executorRewardPkh = pubKeyHashAddress executorPkh Nothing
    exexutorRewardOut = 
        TxOutCandidate
          { txOutCandidateAddress   = executorRewardPkh
          , txOutCandidateValue     = Ada.lovelaceValueOf (exFee - safeTxFeeLovalace)
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut, exexutorRewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just . ReturnTo $ rewardAddr
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp)
  