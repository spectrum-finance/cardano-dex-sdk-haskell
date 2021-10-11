module ErgoDex.Amm.PoolActions
  ( PoolActions(..)
  , OrderExecErr
  , mkPoolActions
  ) where

import           Control.Monad                   (when)
import           Data.Bifunctor
import           Data.Either.Combinators         (maybeToRight)

import           Ledger                          (PubKeyHash(..), Redeemer(..), pubKeyHashAddress)
import qualified Ledger.Typed.Scripts.Validators as Validators
import           Ledger.Address
import           Plutus.V1.Ledger.Value
import qualified Ledger.Ada                      as Ada
import           Ledger.Scripts                  (unitRedeemer, Datum(..), Redeemer(..))
import           PlutusTx                        (toBuiltinData)
import qualified PlutusTx.AssocMap               as Map
import           PlutusTx.Sqrt

import           ErgoDex.Types
import           ErgoDex.State
import           ErgoDex.Amm.Orders
import           ErgoDex.Amm.Pool
import           ErgoDex.Errors
import qualified ErgoDex.Contracts.Pool as P
import           ErgoDex.OffChain
import           ErgoDex.Contracts.Types
import           Cardano.Models
import           Cardano.Utils

data OrderExecErr = PriceTooHigh deriving (Show)

data PoolActions = PoolActions
  { runSwap    :: Confirmed Swap    -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runDeposit :: Confirmed Deposit -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runRedeem  :: Confirmed Redeem  -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
  , runInit    :: [FullTxIn]        -> Confirmed Pool -> Address -> Either TxCandidateCreationErr TxCandidate
  , runDeploy  :: [FullTxIn]        -> P.PoolParams   -> Address -> Either TxCandidateCreationErr TxCandidate
  }

mkPoolActions :: PubKeyHash -> PoolActions
mkPoolActions executorPkh = PoolActions
  { runSwap    = runSwap' executorPkh
  , runDeposit = runDeposit' executorPkh
  , runRedeem  = runRedeem' executorPkh
  , runInit    = runInit'
  , runDeploy  = runDeploy'
  }

runSwap' :: PubKeyHash -> Confirmed Swap -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runSwap' executorPkh (Confirmed swapOut Swap{swapExFee=ExFeePerToken{..}, ..}) (Confirmed poolOut pool) = do
  let
    poolIn  = FullTxIn poolOut Pay2Script (Just $ Redeemer $ toBuiltinData P.Swap)
    orderIn = FullTxIn swapOut Pay2Script (Just unitRedeemer)
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applySwap pool (AssetAmount swapBase swapBaseIn)

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

    pp@(Predicted nextPoolOut _) = applyDeposit pool (inX, inY)

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

runRedeem' :: PubKeyHash -> Confirmed Redeem -> Confirmed Pool -> Either OrderExecErr (TxCandidate, Predicted Pool)
runRedeem' executorPkh (Confirmed redeemOut Redeem{..}) (Confirmed poolOut pool) = do
  let
    poolIn  = FullTxIn poolOut Pay2Script (Just $ Redeemer $ toBuiltinData P.Redeem)
    orderIn = FullTxIn redeemOut Pay2Script (Just unitRedeemer)
    inputs  = [poolIn, orderIn]

    pp@(Predicted nextPoolOut _) = applyRedeem pool redeemLqIn

    executorFee = unExFee redeemExFee
    executorOut = TxOutCandidate
      { txOutCandidateAddress = pubKeyHashAddress executorPkh
      , txOutCandidateValue   = Ada.lovelaceValueOf $ unAmount executorFee
      , txOutCandidateDatum   = Nothing
      }

    rewardOut =
      TxOutCandidate
        { txOutCandidateAddress = pubKeyHashAddress redeemRewardPkh
        , txOutCandidateValue   = rewardValue
        , txOutCandidateDatum   = Nothing
        }
      where
        (outX, outY)    = sharesAmount pool redeemLqIn
        initValue       = fullTxOutValue redeemOut
        valueWithoutFee = lovelaceSubtract initValue (Ada.Lovelace $ unAmount executorFee)
        rewardValue     = (assetAmountValue outX) <> (assetAmountValue outY) <> valueWithoutFee

    outputs = [nextPoolOut, rewardOut, executorOut]

  Right (TxCandidate inputs outputs, pp)

runInit' :: [FullTxIn] -> Confirmed Pool -> Address -> Either TxCandidateCreationErr TxCandidate
runInit' toInput (Confirmed poolOut Pool{..}) changeAddr = do
  let commonValue = foldr ( (<>) . fullTxOutValue . fullTxInTxOut) mempty toInput

  coinX   <-
    maybeToRight (coinNotFoundError poolCoinX) (getAssetAmountFromValueByCoin commonValue poolCoinX)

  coinY   <-
    maybeToRight (coinNotFoundError poolCoinY) (getAssetAmountFromValueByCoin commonValue poolCoinY)

  coinNft <-
    maybeToRight (coinNotFoundError (unPoolId poolId)) (getAssetAmountFromValueByCoin commonValue (unPoolId poolId))

  let
    coinXAmount     = unAmount $ getAmount coinX
    coinYAmount     = unAmount $ getAmount coinY
    outputPoolValue = coinXValue <> coinYValue <> coinNftValue
      where
        coinXValue   = assetAmountValue coinX
        coinYValue   = assetAmountValue coinY
        coinNftValue = assetAmountValue coinNft
    poolRedeemer    = Redeemer $ PlutusTx.toBuiltinData P.Init
    poolInputMaybe  = fromTxOut2TxIn poolOut (Just poolRedeemer)
    inputs          =
      case poolInputMaybe of
        Just p  -> p : toInput
        Nothing -> toInput

  lpAmount <-
    case isqrt (coinXAmount * coinYAmount) of
      Exactly l | l > 0       -> Right l
      Approximately l | l > 0 -> Right $ l + 1
      _                       -> Left incorrectLpAmountError

  let
    outputWithPool =
      TxOutCandidate
        { txOutCandidateAddress = Validators.validatorAddress poolInstance
        , txOutCandidateValue   = outputPoolValue
        , txOutCandidateDatum   = fullTxOutDatum poolOut
        }

    userValue = valueWithAssets <> valueWithLp
      where
        assetClasses2drop = [unCoin $ getAsset coinX, unCoin $ getAsset coinY, unCoin $ getAsset coinNft]
        valueWithAssets   = foldr removeAssetClassFromValue commonValue assetClasses2drop
        valueWithLp       = assetClassValue (unCoin poolCoinLq) lpAmount

    outputs = [outputWithPool, userOutput]
      where
        userOutput =
          TxOutCandidate
            { txOutCandidateAddress = changeAddr
            , txOutCandidateValue   = userValue
            , txOutCandidateDatum   = Nothing
            }

  Right $ TxCandidate { txCandidateInputs = inputs, txCandidateOutputs = outputs }

runDeploy' :: [FullTxIn] -> P.PoolParams -> Address -> Either TxCandidateCreationErr TxCandidate
runDeploy' inputs poolParams@P.PoolParams{..} changeAddr = do
  let commonValue  = foldr ( (<>) . fullTxOutValue . fullTxInTxOut) mempty inputs
  coinNft <- maybeToRight (coinNotFoundError poolNft) (getAssetAmountFromValueByCoin commonValue poolNft)
  let
    userValue = foldr removeAssetClassFromValue commonValue assetClasses2drop
      where
        assetClasses2drop = [unCoin $ getAsset coinNft]
    poolDatum       = Datum $ PlutusTx.toBuiltinData poolParams
    outputPoolValue = assetAmountValue coinNft
    outputWithPool =
      TxOutCandidate
        { txOutCandidateAddress = Validators.validatorAddress poolInstance
        , txOutCandidateValue   = outputPoolValue
        , txOutCandidateDatum   = Just poolDatum
        }
    outputs =
        if (userValueIsEmpty)
        then [outputWithPool]
        else [outputWithPool, userOutput]
      where
        userValueIsEmpty = getValue userValue == Map.empty
        userOutput       =
          TxOutCandidate
            { txOutCandidateAddress = changeAddr
            , txOutCandidateValue   = userValue
            , txOutCandidateDatum   = Nothing
            }

  Right $ TxCandidate { txCandidateInputs = inputs, txCandidateOutputs = outputs }