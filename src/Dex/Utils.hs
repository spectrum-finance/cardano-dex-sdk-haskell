{-# LANGUAGE RecordWildCards #-}

module Dex.Utils where

import           Dex.Models
import           Ledger                           hiding (txOutValue)
import qualified PlutusTx.Builtins                as Builtins
import           Plutus.V1.Ledger.Value
import qualified PlutusTx.AssocMap                as Map
import           Plutus.V1.Ledger.Address

fullTxOut2TxOut :: FullTxOut -> TxOut
fullTxOut2TxOut _ = undefined

generateUserSwapOutput :: SwapOpData -> Pool -> TxOut
generateUserSwapOutput SwapOpData{..} Pool{..} =
    let
        address = pubKeyHashAddress $ PubKeyHash userPubKey
        toSwapTokenSymbol = if (inputTokenSymbol == (tokenXSymbol poolData)) then (tokenYSymbol poolData) else (tokenXSymbol poolData)
        toSwapTokenName = if (inputTokenName == (tokenXName poolData)) then (tokenYName poolData) else (tokenXName poolData)
        currentRate = calculateSwapRate toSwapTokenSymbol toSwapTokenName (txOutValue proxyBox) fullTxOut
        -- value = Value { Map.Map toSwapToken }
    in undefined --TxOut {}

generatePoolSwapOutput :: SwapOpData -> Pool -> TxOut
generatePoolSwapOutput swapData pool = undefined
    -- let

    -- in TxOut {}

generateUserDepositOutput :: DepositOpData -> PoolData -> TxOut
generateUserDepositOutput depositData poolData = undefined
    -- let

    -- in TxOut {}

generatePoolDepositOutput :: DepositOpData -> PoolData -> TxOut
generatePoolDepositOutput depositData poolData = undefined
    -- let

    -- in TxOut {}

generateUserRedeemOutput :: RedeemOpData -> PoolData -> TxOut
generateUserRedeemOutput redeemData poolData = undefined
    -- let

    -- in TxOut {}

generatePoolRedeemOutput :: RedeemOpData -> PoolData -> TxOut
generatePoolRedeemOutput redeemData poolData = undefined
    -- let

    -- in TxOut {}

generateEmptyValue :: Value
generateEmptyValue = Value Map.empty

calculateSwapRate :: Builtins.ByteString -> Builtins.ByteString -> Value -> FullTxOut -> Integer
calculateSwapRate tokenSymbolToSwap tokenNameToSwap boxToSwapValue currentPool = undefined