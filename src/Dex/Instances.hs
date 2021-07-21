{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Dex.Instances where

import           Plutus.V1.Ledger.Tx              hiding (txOutValue)
import           Plutus.V1.Ledger.Value
import qualified PlutusTx.Builtins                as Builtins
import qualified Data.Set                         as Set
import qualified PlutusTx
import           Plutus.V1.Ledger.Scripts
import           Dex.Models                       as ROp    (RedeemOpData(..))
import           Dex.Models                       as DOp    (DepositOpData(..))
import           Dex.Models                       as SOp    (SwapOpData(..))
import           Dex.Models
import           Dex.Utils
import           Dex.Contract.OffChain
import           Dex.Contract.Models
import           Proxy.Contract.Models
import           Proxy.Contract.OnChain
import           Utils                            (PoolId(..))

instance OperationOps (Operation a) where
    getInputs (SwapOperation swapData) Pool{..} =
      let
        swapProxyRedeemer = Redeemer $ PlutusTx.toData Swap
        inputWithProxyTxType = ConsumeScriptAddress proxyValidator swapProxyRedeemer (fullTxOutDatum $ SOp.proxyBox swapData)
        inputWithProxyContract = TxIn {
          txInRef = TxOutRef {
            txOutRefId = refId (SOp.proxyBox swapData),
            txOutRefIdx = refIdx (SOp.proxyBox swapData)
          },
          txInType = Just inputWithProxyTxType
        }
        swapPoolRedeemer = Redeemer $ PlutusTx.toData SwapToken
        inputWithPoolTxType = ConsumeScriptAddress dexValidator swapPoolRedeemer (fullTxOutDatum fullTxOut)
        inputWithPoolContract = TxIn {
          txInRef = TxOutRef {
            txOutRefId = refId $ fullTxOut,
            txOutRefIdx = refIdx $ fullTxOut
          },
          txInType = Just inputWithPoolTxType
        }
      in Set.fromList [inputWithProxyContract, inputWithPoolContract]
    getInputs (DepositOperation depositData) Pool{..} =
      let
        depositProxyRedeemer = Redeemer $ PlutusTx.toData Deposit
        inputWithProxyTxType = ConsumeScriptAddress proxyValidator depositProxyRedeemer (fullTxOutDatum $ DOp.proxyBox depositData)
        inputWithProxyContract = TxIn {
          txInRef = TxOutRef {
            txOutRefId = refId (DOp.proxyBox depositData),
            txOutRefIdx = refIdx (DOp.proxyBox depositData)
          },
          txInType = Just inputWithProxyTxType
        }
        depositPoolRedeemer = Redeemer $ PlutusTx.toData AddTokens
        inputWithPoolTxType = ConsumeScriptAddress dexValidator depositPoolRedeemer (fullTxOutDatum fullTxOut)
        inputWithPoolContract = TxIn {
          txInRef = TxOutRef {
            txOutRefId = refId $ fullTxOut,
            txOutRefIdx = refIdx $ fullTxOut
          },
          txInType = Just inputWithPoolTxType
        }
      in Set.fromList [inputWithProxyContract, inputWithPoolContract]
    getInputs (RedeemOperation redeemData) Pool{..} =
      let
        redeemProxyRedeemer = Redeemer $ PlutusTx.toData Redeem
        inputWithProxyTxType = ConsumeScriptAddress proxyValidator redeemProxyRedeemer (fullTxOutDatum $ ROp.proxyBox redeemData)
        inputWithProxyContract = TxIn {
          txInRef = TxOutRef {
            txOutRefId = refId (ROp.proxyBox redeemData),
            txOutRefIdx = refIdx (ROp.proxyBox redeemData)
          },
          txInType = Just inputWithProxyTxType
        }
        redeemPoolRedeemer = Redeemer $ PlutusTx.toData SwapLP
        inputWithPoolTxType = ConsumeScriptAddress dexValidator redeemPoolRedeemer (fullTxOutDatum fullTxOut)
        inputWithPoolContract = TxIn {
          txInRef = TxOutRef {
            txOutRefId = refId $ fullTxOut,
            txOutRefIdx = refIdx $ fullTxOut
          },
          txInType = Just inputWithPoolTxType
        }
      in Set.fromList [inputWithProxyContract, inputWithPoolContract]
    generateOutputs (SwapOperation swapData) poolToSwap =
        let
            userOutput = generateUserSwapOutput swapData poolToSwap
            poolOutput = generatePoolSwapOutput swapData poolToSwap
        in [userOutput, poolOutput]
    generateOutputs (DepositOperation depositData) pool =
        let
            userOutput = generateUserDepositOutput depositData pool
            poolOutput = generatePoolDepositOutput depositData pool
        in [userOutput, poolOutput]
    generateOutputs (RedeemOperation redeemData) pool =
        let
            userOutput = generateUserRedeemOutput redeemData pool
            poolOutput = generatePoolRedeemOutput redeemData pool
        in [userOutput, poolOutput]
    checkPool (SwapOperation swapData) pool =
        let currentPoolData = poolData pool
            poolValue = txOutValue $ fullTxOut pool
            qtyOfSwapToken = assetClassValueOf poolValue (toSwapCoin swapData)
            qtyOfGetTokenInPool = assetClassValueOf poolValue (toGetCoin swapData)
            isCorrectPoolId = poolId currentPoolData == swapPoolId swapData
            getTokenBoundCheck = qtyOfGetTokenInPool - (minOutputTokenValue swapData) > 0
        in isCorrectPoolId && getTokenBoundCheck
    checkPool (DepositOperation depositData) pool =
        let currentPoolData = poolData pool
            poolValue = txOutValue $ fullTxOut pool
            depositValue = txOutValue $ DOp.proxyBox depositData
            qtyOfCoinXDeposit = assetClassValueOf depositValue (xPoolCoin currentPoolData)
            qtyOfCoinYDeposit = assetClassValueOf depositValue (yPoolCoin currentPoolData)
            qtyOfLpCoinInPool = assetClassValueOf poolValue (lpPoolCoin currentPoolData)
            isCorrectPoolId = poolId currentPoolData == depositPoolId depositData
            reward = rewardLP depositData pool
            lpCoinBoundCheck = qtyOfLpCoinInPool - reward > 0
        in (qtyOfCoinXDeposit > 0) && (qtyOfCoinYDeposit > 0) && lpCoinBoundCheck && isCorrectPoolId

    checkPool (RedeemOperation redeemData) pool =
        let currentPoolData = poolData pool
            poolValue = txOutValue $ fullTxOut pool
            redeemValue = txOutValue $ ROp.proxyBox redeemData
            qtyOfLpCoinToRedeemm = assetClassValueOf redeemValue (lpPoolCoin currentPoolData)
            isCorrectPoolId = poolId currentPoolData == redeemPoolId redeemData
        in (qtyOfLpCoinToRedeemm > 0) && isCorrectPoolId


generateUserSwapOutput :: SwapOpData -> Pool -> TxOut
generateUserSwapOutput SwapOpData{..} Pool{..} =
    let
        boxToSwapValue = txOutValue proxyBox
        poolValue = txOutValue $ fullTxOut
        toSwapXQty = assetClassValueOf boxToSwapValue toSwapCoin
        xQtyInPool = assetClassValueOf poolValue toSwapCoin
        yQtyInPool = assetClassValueOf poolValue toGetCoin
        -- newSwapCoinAmount = xQtyInPool + toSwapXQty
        newGetCoinDelta = xQtyInPool * toSwapXQty * (poolFee poolData) `div` (yQtyInPool * feeDenominator + toSwapXQty * (poolFee poolData))
        newValue = assetClassValue toGetCoin newGetCoinDelta
    in TxOut userAddress newValue Nothing

generatePoolSwapOutput :: SwapOpData -> Pool -> TxOut
generatePoolSwapOutput SwapOpData{..} Pool{..} =
    let
        address = Dex.Models.txOutAddress fullTxOut
        boxToSwapValue = txOutValue proxyBox
        poolValue = txOutValue $ fullTxOut
        toSwapXQty = assetClassValueOf boxToSwapValue toSwapCoin
        xQtyInPool = assetClassValueOf poolValue toSwapCoin
        yQtyInPool = assetClassValueOf poolValue toGetCoin
        lpQtyInPool = assetClassValueOf poolValue (lpPoolCoin poolData)
        newGetCoinDelta = xQtyInPool * toSwapXQty * (poolFee poolData) `div` (yQtyInPool * feeDenominator + toSwapXQty * (poolFee poolData))
        newYValue = assetClassValue toGetCoin (yQtyInPool - newGetCoinDelta)
        newXValue = assetClassValue toSwapCoin (xQtyInPool + toSwapXQty)
        newLpValue = assetClassValue (lpPoolCoin poolData) lpQtyInPool
        newValue = newXValue <> newYValue <> newLpValue
        poolDatum = fullTxOutDatum fullTxOut
        poolDatumHash = datumHash poolDatum
    in TxOut address newValue (Just poolDatumHash)

generateUserDepositOutput :: DepositOpData -> Pool -> TxOut
generateUserDepositOutput depositData pool =
    let reward = rewardLP depositData pool
        userLpValue = assetClassValue (lpPoolCoin $ poolData pool) reward
    in TxOut (DOp.userAddress depositData) userLpValue Nothing

generatePoolDepositOutput :: DepositOpData -> Pool -> TxOut
generatePoolDepositOutput depositData pool =
    let address = Dex.Models.txOutAddress $ fullTxOut pool
        depositValue = txOutValue $ DOp.proxyBox depositData
        poolValue = txOutValue $ fullTxOut pool
        reward = rewardLP depositData pool
        xQtyInBox = assetClassValueOf depositValue (xPoolCoin $ poolData pool)
        yQtyInBox = assetClassValueOf depositValue (yPoolCoin $ poolData pool)
        xQtyInPool = assetClassValueOf poolValue (xPoolCoin $ poolData pool)
        yQtyInPool = assetClassValueOf poolValue (yPoolCoin $ poolData pool)
        lpQtyInPool = assetClassValueOf poolValue (lpPoolCoin $ poolData pool)
        xValue = assetClassValue (xPoolCoin $ poolData pool) (xQtyInBox + xQtyInPool)
        yValue = assetClassValue (yPoolCoin $ poolData pool) (yQtyInBox + yQtyInPool)
        lpValue = assetClassValue (lpPoolCoin $ poolData pool) (lpQtyInPool - reward)
        resultedValue = xValue <> yValue <> lpValue
        poolDatum = fullTxOutDatum $ fullTxOut pool
        poolDatumHash = datumHash poolDatum
    in TxOut address resultedValue (Just poolDatumHash)

rewardLP :: DepositOpData -> Pool -> Integer
rewardLP DepositOpData{..} Pool{..} =
    let depositValue = txOutValue proxyBox
        poolValue = txOutValue fullTxOut
        xQtyInBox = assetClassValueOf depositValue (xPoolCoin poolData)
        yQtyInBox = assetClassValueOf depositValue (yPoolCoin poolData)
        xQtyInPool = assetClassValueOf poolValue (xPoolCoin poolData)
        yQtyInPool = assetClassValueOf poolValue (yPoolCoin poolData)
        lpQtyInPool = assetClassValueOf poolValue (lpPoolCoin poolData)
        currentSupply = totalEmissionLP - lpQtyInPool
        minX = xQtyInBox * currentSupply `div` xQtyInPool
        minY = yQtyInBox * currentSupply `div` yQtyInPool
        result = if (minX < minY) then minX else minY
    in result

generateUserRedeemOutput :: RedeemOpData -> Pool -> TxOut
generateUserRedeemOutput redeemData pool =
    let (shareXQty, shareYQty) = sharesLP redeemData pool
        shareX = assetClassValue (xPoolCoin $ poolData pool) shareXQty
        shareY = assetClassValue (yPoolCoin $ poolData pool) shareYQty
        newValue = shareX <> shareY
    in TxOut (ROp.userAddress redeemData) newValue Nothing

generatePoolRedeemOutput :: RedeemOpData -> Pool -> TxOut
generatePoolRedeemOutput redeemData pool =
    let address = Dex.Models.txOutAddress $ fullTxOut pool
        poolValue = txOutValue $ fullTxOut pool
        proxyBoxValue = txOutValue $ ROp.proxyBox redeemData
        xQtyInPool = assetClassValueOf poolValue (xPoolCoin $ poolData pool)
        yQtyInPool = assetClassValueOf poolValue (yPoolCoin $ poolData pool)
        lpQtyInPool = assetClassValueOf poolValue (lpPoolCoin $ poolData pool)
        lpQtyInBox = assetClassValueOf proxyBoxValue (lpPoolCoin $ poolData pool)
        (shareX, shareY) = sharesLP redeemData pool
        xValue = assetClassValue (xPoolCoin $ poolData pool) (xQtyInPool - shareX)
        yValue = assetClassValue (yPoolCoin $ poolData pool) (yQtyInPool - shareY)
        lpValue = assetClassValue (lpPoolCoin $ poolData pool) (lpQtyInPool + lpQtyInBox)
        newValue = xValue <> yValue <> lpValue
        poolDatum = fullTxOutDatum $ fullTxOut pool
        poolDatumHash = datumHash poolDatum
    in TxOut address newValue (Just poolDatumHash)