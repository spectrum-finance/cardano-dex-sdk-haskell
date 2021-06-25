{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Dex.Models where

import qualified PlutusTx.Builtins   as Builtins
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import           Prelude

newtype PoolId = PoolId Builtins.ByteString

newtype GId = GId Integer

data SwapOpData = SwapOpData {
    poolId :: PoolId
}

data DepositOpData = DepositOpData {
    poolId :: PoolId
}

data RedeemOpData = RedeemOpData {
    poolId :: PoolId
}

data Operation a where
    SwapOperation    :: SwapOpData -> Operation SwapOpData
    DepositOperation :: DepositOpData -> Operation DepositOpData
    RedeemOperation  :: RedeemOpData -> Operation RedeemOpData

data PoolData = PoolData

data Pool = Pool {
    gId :: GId,
    poolData :: PoolData,
    fullTxOut :: FullTxOut
}

data FullTxOut = FullTxOut {
    txOutRefId     :: TxId,
    txOutRefIdx    :: Integer, -- ^ Index into the referenced transaction's outputs
    txOutAddress   :: Address,
    txOutValue     :: Value,
    txOutDatum     :: Datum
}