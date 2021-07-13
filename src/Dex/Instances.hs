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

instance OperationOps (Operation a) where
    getInputs (SwapOperation swapData) Pool{..} =
      let
        swapProxyRedeemer = Redeemer $ PlutusTx.toData Swap
        inputWithProxyTxType = ConsumeScriptAddress dexValidator swapProxyRedeemer (fullTxOutDatum $ SOp.proxyBox swapData)
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
        inputWithProxyTxType = ConsumeScriptAddress dexValidator depositProxyRedeemer (fullTxOutDatum $ DOp.proxyBox depositData)
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
        inputWithProxyTxType = ConsumeScriptAddress dexValidator redeemProxyRedeemer (fullTxOutDatum $ ROp.proxyBox redeemData)
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
            qtyOfSwapToken = assetClassValueOf poolValue (toGetCoin swapData)
            qtyOfGetTokenInPool = assetClassValueOf poolValue (toSwapCoin swapData)
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


    -- Constraints.otherData datum <>
    --                Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
    --                Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)


    -- Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
     --         Constraints.mustPayToTheScript proxyDatum value