{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Dex.Models where

import qualified PlutusTx.Builtins   as Builtins
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import           Prelude
import           GHC.Generics
import           Data.Aeson (FromJSON(..), ToJSON(..))
import           Servant.API

newtype PoolId = PoolId Builtins.ByteString 
    deriving (Show, Generic, FromJSON, ToJSON, FromHttpApiData)

newtype GId = GId { gIx :: Integer }
    deriving (Show, Generic, FromJSON, ToJSON)

data SwapOpData = SwapOpData {
    poolId :: PoolId
} deriving (Show, Generic, FromJSON, ToJSON)

data DepositOpData = DepositOpData {
    poolId :: PoolId
} deriving (Show, Generic, FromJSON, ToJSON)

data RedeemOpData = RedeemOpData {
    poolId :: PoolId
} deriving (Show, Generic, FromJSON, ToJSON)

data Operation a where
    SwapOperation    :: SwapOpData -> Operation SwapOpData
    DepositOperation :: DepositOpData -> Operation DepositOpData
    RedeemOperation  :: RedeemOpData -> Operation RedeemOpData

data PoolData = PoolData deriving (Show, Generic, FromJSON, ToJSON)

data Pool = Pool {
    poolId :: PoolId,
    poolData :: PoolData,
    fullTxOut :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut {
    gId              :: GId,
    txOutRefId       :: TxId, -- add newtype model
    txOutRefIdx      :: Integer, -- ^ Index into the referenced transaction's outputs, add newtype model
    txOutAddress     :: Address,
    txOutValue       :: Value,
    fullTxOutDatum   :: Datum
} deriving (Show, Generic, FromJSON, ToJSON)