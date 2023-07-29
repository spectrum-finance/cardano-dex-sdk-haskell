module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr(..)
  , mkPoolActions
  , AmmValidators(..)
  , fetchValidatorsV1
  ) where

import           Control.Exception.Base
import qualified Data.Set                   as Set
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
import System.Logging.Hlog (Logging (Logging))
import Plutus.V1.Ledger.Value (Value)

data OrderExecErr
  = PriceTooHigh
  | PoolMismatch PoolId PoolId
  | EmptyPool PoolId
  | PoolNotFoundInFinalTx PoolId
  | InsufficientPoolLqForSwap PoolId
  deriving (Show)

instance Exception OrderExecErr

data AmmValidators ver = AmmValidators
  { poolV    :: PoolValidator ver
  , swapV    :: SwapValidator ver
  , depositV :: DepositValidator ver
  , redeemV  :: RedeemValidator ver
  }

-- debug order info
data OrderInfo = RedeemInfo { redeem :: Redeem
                            , redeemOut :: FullTxOut
                            , burnLqValue :: Maybe Value
                            , realExFee :: Maybe Integer
                            , outXAndY :: Maybe (AssetAmount X, AssetAmount Y)
                            , exFee :: Maybe Integer
                            }
               | SwapInfo { swap :: Swap
                          , swapOut :: FullTxOut
                          , realQuoteOutput :: AssetAmount Quote
                          , realExFee :: Maybe Integer
                          , realRV :: Maybe Value
                          }
               | DepositInfo { deposit :: Deposit
                             , depositOut :: FullTxOut
                             , inXAndY :: Maybe (Amount X, Amount Y)
                             , netXAndY :: Maybe (Amount X, Amount Y)
                             , rewardLPAndCharge :: Maybe (Amount Liquidity, (Amount X, Amount Y))
                             , mintLqValue :: Maybe Value
                             , depositExFee :: Maybe (Amount Lovelace)
                             }
                deriving (Show)

fetchValidatorsV1 :: MonadIO m => m (AmmValidators V1)
fetchValidatorsV1 =
  AmmValidators
    <$> fetchPoolValidatorV1
    <*> fetchSwapValidatorV1
    <*> fetchDepositValidatorV1
    <*> fetchRedeemValidatorV1

data PoolActions = PoolActions
  { runSwapWithDebug    :: [FullTxOut] -> OnChain Swap    -> (FullTxOut, Pool) -> Either (OrderExecErr, OrderInfo) (TxCandidate, Predicted Pool, OrderInfo)
  , runDepositWithDebug :: [FullTxOut] -> OnChain Deposit -> (FullTxOut, Pool) -> Either (OrderExecErr, OrderInfo) (TxCandidate, Predicted Pool, OrderInfo)
  , runRedeemWithDebug  :: [FullTxOut] -> OnChain Redeem  -> (FullTxOut, Pool) -> Either (OrderExecErr, OrderInfo) (TxCandidate, Predicted Pool, OrderInfo)
  , runSwap    :: [FullTxOut] -> OnChain Swap    -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool, Integer)
  , runDeposit :: [FullTxOut] -> OnChain Deposit -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool, Integer)
  , runRedeem  :: [FullTxOut] -> OnChain Redeem  -> (FullTxOut, Pool) -> Either OrderExecErr (TxCandidate, Predicted Pool, Integer)
  }

mkPoolActions :: PaymentPubKeyHash -> AmmValidators V1 -> PoolActions
mkPoolActions executorPkh AmmValidators{..} = PoolActions
  { runSwapWithDebug    = runSwapWithDebug' executorPkh poolV swapV
  , runDepositWithDebug = runDepositWithDebug' executorPkh poolV depositV
  , runRedeemWithDebug  = runRedeemWithDebug' executorPkh poolV redeemV
  , runSwap    = runSwapUnsafe' executorPkh poolV swapV
  , runDeposit = runDepositUnsafe' executorPkh poolV depositV
  , runRedeem  = runRedeemUnsafe' executorPkh poolV redeemV
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

runSwapUnsafe'
  :: PaymentPubKeyHash
  -> PoolValidator V1
  -> SwapValidator V1
  -> [FullTxOut]
  -> OnChain Swap
  -> (FullTxOut, Pool)
  -> Either OrderExecErr (TxCandidate, Predicted Pool, Integer)
runSwapUnsafe' executorPkh pv sv refInputs (OnChain swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (poolOut, pool) = do
  let
    inputs = mkOrderInputs P.Swap pv sv (PoolIn poolOut) (OrderIn swapOut)
    pp@(Predicted nextPoolOut _) = applySwap pv pool (AssetAmount swapBase swapBaseIn)
    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)

  when (swapPoolId /= poolId pool)               (Left $ PoolMismatch swapPoolId (poolId pool))
  when (getAmount quoteOutput < swapMinQuoteOut) (Left PriceTooHigh)
  when (poolReservesX pool * 2 <= lqBound pool)  (Left $ InsufficientPoolLqForSwap (poolId pool))

  let
    fee = 300000
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

    executorRewardOut =
      TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = Ada.lovelaceValueOf (exFee - fee)
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut, executorRewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp, exFee - fee)

runDepositUnsafe'
  :: PaymentPubKeyHash
  -> PoolValidator V1
  -> DepositValidator V1
  -> [FullTxOut]
  -> OnChain Deposit
  -> (FullTxOut, Pool)
  -> Either OrderExecErr (TxCandidate, Predicted Pool, Integer)
runDepositUnsafe' executorPkh pv dv refInputs (OnChain depositOut Deposit{..}) (poolOut, pool@Pool{..}) = do
  when (depositPoolId /= poolId) (Left $ PoolMismatch depositPoolId poolId)
  let
    fee = 300000
    inputs = mkOrderInputs P.Deposit pv dv (PoolIn poolOut) (OrderIn depositOut)

    (inX, inY) =
        bimap entryAmount entryAmount $
          if assetEntryClass (fst depositPair) == unCoin poolCoinX
          then depositPair
          else Data.Tuple.swap depositPair
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

    executorRewardOut =
      TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = Ada.lovelaceValueOf (unAmount exFee - fee)
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut, executorRewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp, unAmount exFee - fee)

runRedeemUnsafe'
  :: PaymentPubKeyHash
  -> PoolValidator V1
  -> RedeemValidator V1
  -> [FullTxOut]
  -> OnChain Redeem
  -> (FullTxOut, Pool)
  -> Either OrderExecErr (TxCandidate, Predicted Pool, Integer)
runRedeemUnsafe' executorPkh pv rv refInputs (OnChain redeemOut Redeem{..}) (poolOut, pool@Pool{..}) = do
  when (redeemPoolId /= poolId) (Left $ PoolMismatch redeemPoolId poolId)
  let
    fee = 300000

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

    executorRewardOut =
      TxOutCandidate
          { txOutCandidateAddress   = rewardAddr
          , txOutCandidateValue     = Ada.lovelaceValueOf (exFee - fee)
          , txOutCandidateDatum     = EmptyDatum
          , txOutCandidateRefScript = Nothing
          }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut, executorRewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp, exFee - fee)

runSwapWithDebug'
  :: PaymentPubKeyHash
  -> PoolValidator V1
  -> SwapValidator V1
  -> [FullTxOut]
  -> OnChain Swap
  -> (FullTxOut, Pool)
  -> Either (OrderExecErr, OrderInfo) (TxCandidate, Predicted Pool, OrderInfo)
runSwapWithDebug' executorPkh pv sv refInputs (OnChain swapOut s@Swap{swapExFee=ExFeePerToken{..}, ..}) (poolOut, pool) = do
  let
    inputs = mkOrderInputs P.Swap pv sv (PoolIn poolOut) (OrderIn swapOut)
    pp@(Predicted nextPoolOut _) = applySwap pv pool (AssetAmount swapBase swapBaseIn)
    quoteOutput = outputAmount pool (AssetAmount swapBase swapBaseIn)
    initSwapInfo = SwapInfo
      { swap = s
      , swapOut = swapOut
      , realQuoteOutput = quoteOutput
      , realExFee = Nothing
      , realRV = Nothing
      }
  when (swapPoolId /= poolId pool)               (Left $ (PoolMismatch swapPoolId (poolId pool), initSwapInfo))
  when (getAmount quoteOutput < swapMinQuoteOut) (Left (PriceTooHigh, initSwapInfo))
  when (poolReservesX pool * 2 <= lqBound pool)  (Left $ (InsufficientPoolLqForSwap (poolId pool), initSwapInfo))

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

    fullSwapInfo = SwapInfo
      { swap = s
      , swapOut = swapOut
      , realQuoteOutput = quoteOutput
      , realExFee = Just exFee
      , realRV = Just (txOutCandidateValue rewardOut)
      }

    txCandidate = TxCandidate
        { txCandidateInputs       = inputs
        , txCandidateRefIns       = refInputs
        , txCandidateOutputs      = [nextPoolOut, rewardOut]
        , txCandidateValueMint    = mempty
        , txCandidateMintInputs   = mempty
        , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
        , txCandidateValidRange   = Interval.always
        , txCandidateSigners      = mempty
        }

  Right (txCandidate, pp, fullSwapInfo)

runDepositWithDebug'
  :: PaymentPubKeyHash
  -> PoolValidator V1
  -> DepositValidator V1
  -> [FullTxOut]
  -> OnChain Deposit
  -> (FullTxOut, Pool)
  -> Either (OrderExecErr, OrderInfo) (TxCandidate, Predicted Pool, OrderInfo)
runDepositWithDebug' executorPkh pv dv refInputs (OnChain depositOut d@Deposit{..}) (poolOut, pool@Pool{..}) = do
  let
    initDepositInfo = DepositInfo
      { deposit = d
      , depositOut = depositOut
      , inXAndY = Nothing
      , netXAndY = Nothing
      , rewardLPAndCharge = Nothing
      , mintLqValue = Nothing
      , depositExFee = Nothing
      }

  when (depositPoolId /= poolId) (Left $ ((PoolMismatch depositPoolId poolId), initDepositInfo))
  let
    inputs = mkOrderInputs P.Deposit pv dv (PoolIn poolOut) (OrderIn depositOut)

    (inX, inY) =
        bimap entryAmount entryAmount $
          if assetEntryClass (fst depositPair) == unCoin poolCoinX
          then depositPair
          else Data.Tuple.swap depositPair
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

    finalDepositInfo = DepositInfo
      { deposit = d
      , depositOut = depositOut
      , inXAndY = Just (inX, inY)
      , netXAndY = Just (netInX, netInY)
      , rewardLPAndCharge = Just $ rewardLp pool (netInX, netInY)
      , mintLqValue = Just mintLqValue
      , depositExFee = Just exFee
      }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp, finalDepositInfo)

runRedeemWithDebug'
  :: PaymentPubKeyHash
  -> PoolValidator V1
  -> RedeemValidator V1
  -> [FullTxOut]
  -> OnChain Redeem
  -> (FullTxOut, Pool)
  -> Either (OrderExecErr, OrderInfo) (TxCandidate, Predicted Pool, OrderInfo)
runRedeemWithDebug' executorPkh pv rv refInputs (OnChain redeemOut r@Redeem{..}) (poolOut, pool@Pool{..}) = do
  let
    initRedeemInfo = RedeemInfo
      { redeem = r
      , redeemOut = redeemOut
      , burnLqValue = Nothing
      , realExFee = Nothing
      , outXAndY = Nothing
      , exFee = Nothing
      }
  when (redeemPoolId /= poolId) (Left $ ((PoolMismatch redeemPoolId poolId), initRedeemInfo))
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

    finalRedeemInfo = RedeemInfo
      { redeem = r
      , redeemOut = redeemOut
      , burnLqValue = Just burnLqValue
      , realExFee = Just exFee
      , outXAndY = Just (sharesAmount pool redeemLqIn)
      , exFee = Just exFee
      }

    txCandidate = TxCandidate
      { txCandidateInputs       = inputs
      , txCandidateRefIns       = refInputs
      , txCandidateOutputs      = [nextPoolOut, rewardOut]
      , txCandidateValueMint    = mempty
      , txCandidateMintInputs   = mempty
      , txCandidateChangePolicy = Just $ ReturnTo $ pubKeyHashAddress executorPkh Nothing
      , txCandidateValidRange   = Interval.always
      , txCandidateSigners      = mempty
      }

  Right (txCandidate, pp, finalRedeemInfo)
