{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GADTs                 #-}

module Dex.Interpreter where

import           Control.Monad.Freer
import           Dex.Models
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Scripts         (Redeemer(..))
import           Ledger.Constraints               as Constraints
import qualified PlutusTx.Builtins                as Builtins
import           Data.Either.Combinators          (maybeToRight)
import           Dex.Utils
import           PlutusTx.IsData
import qualified PlutusTx
import qualified Data.Map                         as Map
import qualified Ledger.Typed.Scripts             as Scripts
import           Plutus.Contract                  hiding (when)
import           Proxy.Contract.Models
import           Proxy.Contract.OnChain
import           Dex.Contract.OffChain
import           Dex.Instances
import           Wallet.Emulator.Wallet
import           Wallet.Effects                   (WalletEffect(..))

--todo: lift MkTxError to dex error
interpretOp :: Operation a -> Pool -> Either MkTxError (Tx, TxOut)
interpretOp op pool =
    do
        unbalancedTx <- createUnbalancedTx op pool
        tx <- makeTxFromUnbalanced unbalancedTx
        -- todo: use correct error
        newPoolOutput <- maybeToRight TypedValidatorMissing (getNewPoolOut tx)
        let result = Right (tx, fullTxOut2TxOut newPoolOutput)
        result

--todo: lift MkTxError to dex error. Set correct errors. Now wip
createUnbalancedTx :: (Operation a) -> Pool -> Either MkTxError UnbalancedTx
createUnbalancedTx operation pool
    | checkPool operation pool /= True = Left TypedValidatorMissing
    -- | swapPoolId /= (poolId poolData) = Left TypedValidatorMissing -- check that poolid is correct
    -- | checkPoolContainsToken inputTokenSymbol inputTokenName poolData /= True = Left TypedValidatorMissing -- check that pool contains token to swap
    | otherwise = let
        value = getValue operation
        lookups = generatePlutusTxLookups operation
        redeemer = generateRedeemer operation
        tx = generatePlutusTxConstraints operation
    in Constraints.mkTx @ProxySwapping lookups tx

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

makeTxFromUnbalanced :: UnbalancedTx -> Either MkTxError Tx
makeTxFromUnbalanced unTx =
    run . handleWallet (balanceTx unTx)

getNewPoolOut :: Tx -> Maybe FullTxOut
getNewPoolOut _ = undefined

checkPoolContainsToken :: Builtins.ByteString -> Builtins.ByteString -> PoolData -> Bool
checkPoolContainsToken inputTokenSymbol inputTokenName pool = undefined