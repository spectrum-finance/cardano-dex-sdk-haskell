{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Dex.Models where

import qualified PlutusTx.Builtins   as Builtins
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import           Prelude

newtype PoolId = PoolId Builtins.ByteString deriving (Show)

newtype GId = GId Integer deriving (Show)

data SwapOpData = SwapOpData {
    poolId :: PoolId
} deriving (Show)

data DepositOpData = DepositOpData {
    poolId :: PoolId
} deriving (Show)

data RedeemOpData = RedeemOpData {
    poolId :: PoolId
} deriving (Show)

data Operation a where
    SwapOperation    :: SwapOpData -> Operation SwapOpData
    DepositOperation :: DepositOpData -> Operation DepositOpData
    RedeemOperation  :: RedeemOpData -> Operation RedeemOpData

data PoolData = PoolData deriving (Show)

data Pool = Pool {
    poolId :: PoolId,
    poolData :: PoolData,
    fullTxOut :: FullTxOut
} deriving (Show)

data FullTxOut = FullTxOut {
    gId :: GId,
    txOutRefId     :: TxId, -- add newtype model
    txOutRefIdx    :: Integer, -- ^ Index into the referenced transaction's outputs, add newtype model
    txOutAddress   :: Address,
    txOutValue     :: Value,
    txOutDatum     :: Datum
} deriving (Show)