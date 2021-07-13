{-# LANGUAGE RecordWildCards #-}

module Dex.Utils where

import           Dex.Models as ROp  (RedeemOpData(..))
import           Dex.Models as DOp  (DepositOpData(..))
import           Dex.Models as SOp  (SwapOpData(..))
import           Dex.Models
import           Ledger                           hiding (txOutValue)
import qualified PlutusTx.Builtins                as Builtins
import           Plutus.V1.Ledger.Value as Value
import qualified PlutusTx.AssocMap                as Map
import           Plutus.V1.Ledger.Address
import           Dex.Contract.Models

fullTxOut2TxOut :: FullTxOut -> TxOut
fullTxOut2TxOut _ = undefined

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

totalEmissionLP :: Integer
totalEmissionLP = 0x7fffffffffffffff

sharesLP :: RedeemOpData -> Pool -> (Integer, Integer)
sharesLP RedeemOpData{..} Pool{..} =
    let poolValue = txOutValue fullTxOut
        proxyBoxValue = txOutValue proxyBox
        lpQtyInPool = assetClassValueOf poolValue (lpPoolCoin poolData)
        lpQtyInBox = assetClassValueOf proxyBoxValue (lpPoolCoin poolData)
        xQtyInPool = assetClassValueOf poolValue (xPoolCoin poolData)
        yQtyInPool = assetClassValueOf poolValue (yPoolCoin poolData)
        supplyLP = totalEmissionLP - lpQtyInPool
        xValue = lpQtyInBox * xQtyInPool `div` supplyLP
        yValue = lpQtyInBox * yQtyInPool `div` supplyLP
    in (xValue, yValue)

generateEmptyValue :: Value
generateEmptyValue = Value Map.empty

feeDenominator :: Integer
feeDenominator = 1000

getPoolId :: ErgoDexPool -> PoolId
getPoolId pool = undefined