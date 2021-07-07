{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE GADTs                 #-}

module Dex.Instances where

import           Plutus.V1.Ledger.Tx
import qualified PlutusTx.Builtins                as Builtins
import qualified Data.Set                         as Set
import           Dex.Models
import           Dex.Utils

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
    checkPool (SwapOperation swapData) pool = undefined
    checkPool (DepositOperation depositData) pool = undefined
    checkPool (RedeemOperation redeemData) pool = undefined

checkPoolContainsToken :: Builtins.ByteString -> Builtins.ByteString -> PoolData -> Bool
checkPoolContainsToken inputTokenSymbol inputTokenName pool = undefined
    -- Constraints.otherData datum <>
    --                Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
    --                Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)


    -- Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
     --         Constraints.mustPayToTheScript proxyDatum value