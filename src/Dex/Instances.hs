{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE GADTs                 #-}

module Dex.Instances where

import           Plutus.V1.Ledger.Tx
import qualified PlutusTx.Builtins                as Builtins
import qualified Data.Set                         as Set
import           Dex.Models
import           Dex.Utils
import           Dex.Models as ROp  (RedeemOpData(..))
import           Dex.Models  as Models
import           Dex.Models as ROp  (RedeemOpData(..))
import           Dex.Models as DOp  (DepositOpData(..))
import           Dex.Models
import           Ledger                           hiding (txOutValue)
import qualified PlutusTx.Builtins                as Builtins
import           Plutus.V1.Ledger.Value as Value
import qualified PlutusTx.AssocMap                as Map
import           Plutus.V1.Ledger.Address

instance OperationOps (Operation a) where
    getInputs (SwapOperation swapData) = undefined
    getInputs (DepositOperation depositData) = undefined
    getInputs (RedeemOperation redeemData) = undefined
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
            poolValue = Models.txOutValue $ fullTxOut pool
            isCorrectPoolId = poolId currentPoolData == swapPoolId swapData
            isCorrectXTokenSymbol = inputtokenXNameTokenName swapData == tokenYName currentPoolData
            poolXTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            poolYTokenValue = valueOf poolValue (CurrencySymbol $ tokenXSymbol currentPoolData) (TokenName $ tokenXName currentPoolData)
            isCorrectXAmount = poolXTokenValue - (minOutputTokenValue swapData) > 0
            isCorrectYAmount = poolYTokenValue - (minOutputTokenValue swapData) > 0
        in (isCorrectXAmount || isCorrectYAmount) && ((isCorrectXTokenSymbol && isCorrectXTokenName) || (isCorrectYTokenSymbol && isCorrectYTokenName))
    checkPool (DepositOperation depositData) pool =
        let currentPoolData = poolData pool
            poolValue = Models.txOutValue $ fullTxOut pool
            isCorrectXTokenSymbol = inputTokenXSymbol depositData == tokenXSymbol currentPoolData
            isCorrectXTokenName = inputTokenXName depositData == tokenLPName currentPoolData
            isCorrectYTokenSymbol = inputTokenYSymbol depositData == tokenYSymbol currentPoolData
            isCorrectYTokenName = inputTokenYName depositData == tokenYName currentPoolData
            isCorrectPoolId = poolId currentPoolData == depositPoolId depositData
            reward = rewardLP depositData pool
            poolLPTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            isCorrectAmount = poolLPTokenValue - reward > 0
        in isCorrectXTokenSymbol && isCorrectXTokenName && isCorrectYTokenSymbol && isCorrectYTokenName && isCorrectPoolId && isCorrectAmount

    checkPool (RedeemOperation redeemData) pool =
        let currentPoolData = poolData pool
            poolValue = Models.txOutValue $ fullTxOut pool
            redeemValue = Models.txOutValue $ ROp.proxyBox redeemData
            isCorrectLPTokenSymbol = lpTokenSymbol redeemData == tokenLPSymbol currentPoolData
            isCorrectLPTokenName = lpTokenName redeemData == tokenLPName currentPoolData
            isCorrectPoolId = poolId currentPoolData == redeemPoolId redeemData
            poolLPTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            poolXTokenValue = valueOf poolValue (CurrencySymbol $ tokenLPSymbol currentPoolData) (TokenName $ tokenLPName currentPoolData)
            poolYTokenValue = valueOf poolValue (CurrencySymbol $ tokenXSymbol currentPoolData) (TokenName $ tokenXName currentPoolData)
            redeemLPTokenValue = valueOf poolValue (CurrencySymbol $ tokenYSymbol currentPoolData) (TokenName $ tokenYName currentPoolData)
            (shareX, shareY) = sharesLP redeemData pool
            isCorrectXAmount = poolXTokenValue - shareX > 0
            isCorrectYAmount = poolYTokenValue - shareY > 0
        in isCorrectLPTokenSymbol && isCorrectLPTokenName && isCorrectXAmount && isCorrectYAmount && isCorrectPoolId

checkPoolContainsToken :: Builtins.ByteString -> Builtins.ByteString -> PoolData -> Bool
checkPoolContainsToken inputTokenSymbol inputTokenName pool = undefined
    -- Constraints.otherData datum <>
    --                Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
    --                Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)


    -- Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
     --         Constraints.mustPayToTheScript proxyDatum value