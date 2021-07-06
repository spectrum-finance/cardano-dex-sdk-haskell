{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GADTs                 #-}

module Dex.Interpreter where

import           Dex.Models
import           Plutus.V1.Ledger.Tx
import           Ledger.Constraints               as Constraints
import qualified PlutusTx.Builtins                as Builtins
import           Data.Either.Combinators          (maybeToRight)
import           Dex.Utils
import           PlutusTx.IsData
import qualified PlutusTx
import qualified Data.Map                         as Map
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract                  hiding (when)
import           Proxy.Contract.OnChain
import           Dex.Contract.OffChain

--todo: lift MkTxError to dex error
interpretOp :: Operation a -> Pool -> Either MkTxError (Tx, TxOut)
interpretOp op pool =
    do
        unbalancedTx <- case op of
                        swap@ (SwapOperation swapData) -> createSwapTransaction swapData pool
                        deposit@ (DepositOperation depositData) -> createDepositTransaction deposit pool
                        redeem@ (RedeemOperation redeemData) -> createRedeemTransaction redeem pool
        tx <- balanceTx unbalancedTx
        -- todo: use correct error
        newPoolOutput <- maybeToRight TypedValidatorMissing (getNewPoolOut tx)
        let result = Right (tx, fullTxOut2TxOut newPoolOutput)
        result

--todo: lift MkTxError to dex error. Set correct errors. Now wip
createSwapTransaction :: SwapOpData -> Pool -> Either MkTxError UnbalancedTx
createSwapTransaction _ _ = undefined
-- createSwapTransaction SwapOpData{..} Pool{..}
--     | swapPoolId /= (poolId poolData) = Left TypedValidatorMissing -- check that poolid is correct
--     | checkPoolContainsToken inputTokenSymbol inputTokenName poolData /= True = Left TypedValidatorMissing -- check that pool contains token to swap
--     | otherwise = let
--         value = lovelaceValueOf 10
--         lookups  = Constraints.otherData datum <>
--                    Constraints.otherScript (Scripts.validatorScript proxyInstance) <>
--                    Constraints.unspentOutputs (Map.singleton proxyTxOutRef o)

--         redeemer = Redeemer $ PlutusTx.toData Swap

--         tx =  Constraints.mustSpendScriptOutput proxyTxOutRef redeemer <>
--               Constraints.mustPayToTheScript proxyDatum value

--         unTx = Constraints.mkTx @ProxySwapping lookups tx
--     in unTx

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

createDepositTransaction :: Operation DepositOpData -> Pool -> Either MkTxError UnbalancedTx
createDepositTransaction _ = undefined

createRedeemTransaction :: Operation RedeemOpData -> Pool -> Either MkTxError UnbalancedTx
createRedeemTransaction _ = undefined

balanceTx :: UnbalancedTx -> Either MkTxError Tx
balanceTx _ = undefined

getNewPoolOut :: Tx -> Maybe FullTxOut
getNewPoolOut _ = undefined

checkPoolContainsToken :: Builtins.ByteString -> Builtins.ByteString -> PoolData -> Bool
checkPoolContainsToken inputTokenSymbol inputTokenName pool = undefined