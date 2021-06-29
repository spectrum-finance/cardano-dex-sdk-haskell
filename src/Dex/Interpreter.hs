{-# LANGUAGE RecordWildCards #-}

module Dex.Interpreter where

import           Dex.Models
import           Plutus.V1.Ledger.Tx
import           Ledger.Constraints               as Constraints

interpretSwapOp :: Operation SwapOpData -> Pool -> Either MkTxError (Tx, TxOut)
interpretSwapOp op pool = undefined

interpretRedeemOp :: Operation RedeemOpData -> Pool -> Either MkTxError (Tx, TxOut)
interpretRedeemOp op pool = undefined

interpretDepositOp :: Operation DepositOpData -> Pool -> Either MkTxError (Tx, TxOut)
interpretDepositOp op pool = undefined

createSwapTransaction :: Operation SwapOpData -> Either MkTxError UnbalancedTx
createSwapTransaction _ = undefined
-- createSwapTransaction proxyTxOutRef proxyDatum datum o =
    -- let
    --     value = lovelaceValueOf 10
    --     lookups  = Constraints.otherData datum <>
    --                Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
    --                Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)

    --     redeemer = Redeemer $ PlutusTx.toData Swap

    --     tx =  Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
    --           Constraints.mustPayToTheScript proxyDatum value

    --     unTx = Constraints.mkTx @ProxySwapping lookups tx
    -- in unTx

createDepositTransaction :: Operation DepositOpData -> Either MkTxError UnbalancedTx
createDepositTransaction _ = undefined

createRedeemTransaction :: Operation RedeemOpData -> Either MkTxError UnbalancedTx
createRedeemTransaction _ = undefined

balanceTx :: UnbalancedTx -> Tx
balanceTx _ = undefined

getNewPoolOut :: Tx -> Maybe FullTxOut
getNewPoolOut _ = undefined
