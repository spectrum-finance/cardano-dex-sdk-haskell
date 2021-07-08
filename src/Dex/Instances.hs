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
            isCorrectPoolId = poolId currentPoolData == swapPoolId swapData
            isCorrectXTokenSymbol = inputTokenSymbol swapData == tokenXSymbol currentPoolData
            isCorrectXTokenName = inputTokenName swapData == tokenXName currentPoolData
            isCorrectYTokenSymbol = inputTokenSymbol swapData == tokenYSymbol currentPoolData
            isCorrectYTokenName = inputTokenName swapData == tokenYName currentPoolData
            poolXTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            poolYTokenValue = valueOf poolValue (CurrencySymbol $ tokenXSymbol currentPoolData) (TokenName $ tokenXName currentPoolData)
            isCorrectXAmount = poolXTokenValue - (minOutputTokenValue swapData) > 0
            isCorrectYAmount = poolYTokenValue - (minOutputTokenValue swapData) > 0
        in (isCorrectXAmount || isCorrectYAmount) && ((isCorrectXTokenSymbol && isCorrectXTokenName) || (isCorrectYTokenSymbol && isCorrectYTokenName))
    checkPool (DepositOperation depositData) pool =
        let currentPoolData = poolData pool
            poolValue = txOutValue $ fullTxOut pool
            isCorrectXTokenSymbol = inputTokenXSymbol depositData == tokenXSymbol currentPoolData
            isCorrectXTokenName = inputTokenXName depositData == tokenXName currentPoolData
            isCorrectYTokenSymbol = inputTokenYSymbol depositData == tokenYSymbol currentPoolData
            isCorrectYTokenName = inputTokenYName depositData == tokenYName currentPoolData
            isCorrectPoolId = poolId currentPoolData == depositPoolId depositData
            reward = rewardLP depositData pool
            poolLPTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            isCorrectAmount = poolLPTokenValue - reward > 0
        in isCorrectXTokenSymbol && isCorrectXTokenName && isCorrectYTokenSymbol && isCorrectYTokenName && isCorrectPoolId && isCorrectAmount

    checkPool (RedeemOperation redeemData) pool =
        let currentPoolData = poolData pool
            poolValue = txOutValue $ fullTxOut pool
            redeemValue = txOutValue $ ROp.proxyBox redeemData
            isCorrectLPTokenSymbol = ROp.lpTokenSymbol redeemData == tokenLPSymbol currentPoolData
            lpTokenNameCheck = ROp.lpTokenName redeemData == tokenLPName currentPoolData
            isCorrectPoolId = poolId currentPoolData == redeemPoolId redeemData
            poolLPTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            poolXTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            poolYTokenValue = valueOf poolValue (CurrencySymbol $ tokenXSymbol currentPoolData) (TokenName $ tokenXName currentPoolData)
            redeemLPTokenValue = valueOf poolValue (CurrencySymbol $ tokenYSymbol currentPoolData) (TokenName $ tokenYName currentPoolData)
            (shareX, shareY) = sharesLP redeemData pool
            isCorrectXAmount = poolXTokenValue - shareX > 0
            isCorrectYAmount = poolYTokenValue - shareY > 0
        in isCorrectLPTokenSymbol && lpTokenNameCheck && isCorrectXAmount && isCorrectYAmount && isCorrectPoolId


--checkPoolContainsToken :: Builtins.ByteString -> Builtins.ByteString -> PoolData -> Bool
--checkPoolContainsToken inputTokenSymbol inputTokenName pool = undefined
--    -- Constraints.otherData datum <>
    --                Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
    --                Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)


    -- Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
     --         Constraints.mustPayToTheScript proxyDatum value