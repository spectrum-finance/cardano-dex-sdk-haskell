{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}

module Dex.Models where

import qualified PlutusTx.Builtins                as Builtins
import qualified Data.Set                         as Set
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import           Ledger.Typed.Scripts             (TypedValidator, ValidatorTypes (..))
import           Playground.Contract              (FromJSON, Generic, ToJSON, ToSchema)
import           Ledger.Constraints

newtype PoolId = PoolId Builtins.ByteString
    deriving (Show, Generic, FromJSON, ToJSON, Eq)

newtype GId = GId Integer
    deriving (Show, Generic, FromJSON, ToJSON)

data SwapOpData = SwapOpData {
    swapPoolId :: PoolId,
    inputTokenSymbol :: Builtins.ByteString,
    inputTokenName :: Builtins.ByteString,
    minOutputTokenValue :: Integer,
    dexFee :: Integer,
    userPubKey :: Builtins.ByteString,
    proxyBox :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data DepositOpData = DepositOpData{
    depositPoolId :: PoolId,
    inputTokenXSymbol :: Builtins.ByteString,
    inputTokenXName :: Builtins.ByteString,
    inputTokenYSymbol :: Builtins.ByteString,
    inputTokenYName :: Builtins.ByteString,
    dexFee :: Integer,
    userPubKey :: Builtins.ByteString,
    proxyBox :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data RedeemOpData = RedeemOpData {
    redeemPoolId :: PoolId,
    lpTokenSymbol :: Builtins.ByteString,
    lpTokenName :: Builtins.ByteString,
    dexFee :: Integer,
    userPubKey :: Builtins.ByteString,
    proxyBox :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data Operation a where
    SwapOperation    :: SwapOpData -> Operation SwapOpData
    DepositOperation :: DepositOpData -> Operation DepositOpData
    RedeemOperation  :: RedeemOpData -> Operation RedeemOpData

data ParsedOperation = forall a. ParsedOperation { op :: Operation a }

data PoolData = PoolData {
    poolId :: PoolId,
    poolFee :: Integer,
    tokenXSymbol :: Builtins.ByteString,
    tokenXName :: Builtins.ByteString,
    tokenYSymbol :: Builtins.ByteString,
    tokenYName :: Builtins.ByteString,
    tokenLPSymbol :: Builtins.ByteString,
    tokenLPName :: Builtins.ByteString
} deriving (Show, Generic, FromJSON, ToJSON)

data Pool = Pool {
    gId :: GId,
    poolData :: PoolData,
    fullTxOut :: FullTxOut
} deriving (Show, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut {
    txOutRefId       :: TxId,
    txOutRefIdx      :: Integer, -- ^ Index into the referenced transaction's outputs
    txOutAddress     :: Address,
    txOutValue       :: Value,
    fullTxOutDatum   :: Datum
} deriving (Show, Generic, FromJSON, ToJSON)

--todo:
class OperationOps a where
    getInputs :: a -> Set.Set TxIn
    generateOutputs :: a -> Pool -> [TxOut]
    checkPool :: a -> Pool -> Bool