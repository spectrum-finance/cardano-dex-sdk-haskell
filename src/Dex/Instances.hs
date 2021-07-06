{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE GADTs                 #-}

module Dex.Instances where

import Dex.Models

instance OperationOps (Operation a) where
    getDatum (SwapOperation swapData) = undefined
    getDatum (DepositOperation depositData) = undefined
    getDatum (RedeemOperation redeemData) = undefined
    getValue (SwapOperation swapData) = undefined
    getValue (DepositOperation depositData) = undefined
    getValue (RedeemOperation redeemData) = undefined
    generateRedeemer (SwapOperation swapData) = undefined
    generateRedeemer (DepositOperation depositData) = undefined
    generateRedeemer (RedeemOperation redeemData) = undefined
    checkPool (SwapOperation swapData) pool = undefined
    checkPool (DepositOperation depositData) pool = undefined
    checkPool (RedeemOperation redeemData) pool = undefined

    -- Constraints.otherData datum <>
    --                Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
    --                Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)


    -- Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
     --         Constraints.mustPayToTheScript proxyDatum value