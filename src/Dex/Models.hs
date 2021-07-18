{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Dex.Models where

import qualified PlutusTx.Builtins                as Builtins
import qualified Data.Set                         as Set
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import           Plutus.V1.Ledger.Crypto
import           Ledger.Typed.Scripts             (TypedValidator, ValidatorTypes (..))
import           Playground.Contract              (FromJSON, Generic, ToJSON, ToSchema)
import           Ledger.Constraints
import           Utils

newtype GId = GId { gIdx :: Integer }
    deriving (Show, Generic, FromJSON, ToJSON)

data SwapOpData = SwapOpData {
    swapPoolId          :: PoolId,
    toSwapCoin          :: AssetClass,
    toGetCoin           :: AssetClass,
    minOutputTokenValue :: Integer,
    dexFee              :: Integer,
    userAddress         :: Address,
    proxyBox            :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data DepositOpData = DepositOpData{
    depositPoolId   :: PoolId,
    depositXCoin    :: AssetClass,
    depositYCoin    :: AssetClass,
    dexFee          :: Integer,
    userAddress     :: Address,
    proxyBox        :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data RedeemOpData = RedeemOpData {
    redeemPoolId    :: PoolId,
    redeemLpCoin    :: AssetClass,
    dexFee          :: Integer,
    userAddress     :: Address,
    proxyBox        :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data Operation a where
    SwapOperation    :: SwapOpData -> Operation SwapOpData
    DepositOperation :: DepositOpData -> Operation DepositOpData
    RedeemOperation  :: RedeemOpData -> Operation RedeemOpData

data ParsedOperation = forall a. ParsedOperation { op :: Operation a }

data PoolData = PoolData {
    poolId      :: PoolId,
    poolFee     :: Integer,
    xPoolCoin   :: AssetClass,
    yPoolCoin   :: AssetClass,
    lpPoolCoin  :: AssetClass
} deriving (Show, Generic, FromJSON, ToJSON)

data Pool = Pool {
    poolGId   :: GId,
    poolData  :: PoolData,
    fullTxOut :: FullTxOut,
    poolTxIn  :: TxOutRef
} deriving (Show, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut {
    outGId           :: GId,
    refId            :: TxId,
    refIdx           :: Integer, -- ^ Index into the referenced transaction's outputs
    txOutAddress     :: Address,
    txOutValue       :: Value,
    fullTxOutDatum   :: Datum
} deriving (Show, Generic, FromJSON, ToJSON)

--todo:
class OperationOps a where
    getInputs :: a -> Pool -> Set.Set TxIn
    generateOutputs :: a -> Pool -> [TxOut]
    checkPool :: a -> Pool -> Bool