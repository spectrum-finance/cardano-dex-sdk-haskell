{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}

module Dex.Models where

import qualified PlutusTx.Builtins   as Builtins
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts

newtype PoolId = PoolId Builtins.ByteString deriving Eq

newtype GId = GId Integer

data SwapOpData = SwapOpData {
    swapPoolId :: PoolId,
    inputTokenSymbol :: Builtins.ByteString,
    inputTokenName :: Builtins.ByteString,
    minOutputTokenValue :: Integer,
    dexFee :: Integer,
    userPubKey :: Builtins.ByteString,
    proxyBox :: FullTxOut
}

data DepositOpData = DepositOpData{
    depositPoolId :: PoolId,
    inputTokenXSymbol :: Builtins.ByteString,
    inputTokenXName :: Builtins.ByteString,
    inputTokenYSymbol :: Builtins.ByteString,
    inputTokenYName :: Builtins.ByteString,
    dexFee :: Integer,
    userPubKey :: Builtins.ByteString,
    proxyBox :: FullTxOut
}

data RedeemOpData = RedeemOpData {
    redeemPoolId :: PoolId,
    lpTokenSymbol :: Builtins.ByteString,
    lpTokenName :: Builtins.ByteString,
    dexFee :: Integer,
    userPubKey :: Builtins.ByteString,
    proxyBox :: FullTxOut
}

data Operation a where
    SwapOperation    :: SwapOpData -> Operation SwapOpData
    DepositOperation :: DepositOpData -> Operation DepositOpData
    RedeemOperation  :: RedeemOpData -> Operation RedeemOpData

data ParsedOperation = forall a. ParsedOperation { op :: Operation a }

data PoolData = PoolData {
    poolId :: PoolId,
    tokenXSymbol :: Builtins.ByteString,
    tokenXName :: Builtins.ByteString,
    tokenYSymbol :: Builtins.ByteString,
    tokenYName :: Builtins.ByteString,
    tokenLPSymbol :: Builtins.ByteString,
    tokenLPName :: Builtins.ByteString
}

data Pool = Pool {
    gId :: GId,
    poolData :: PoolData,
    fullTxOut :: FullTxOut
}

data FullTxOut = FullTxOut {
    txOutRefId       :: TxId,
    txOutRefIdx      :: Integer, -- ^ Index into the referenced transaction's outputs
    txOutAddress     :: Address,
    txOutValue       :: Value,
    fullTxOutDatum   :: Datum
}