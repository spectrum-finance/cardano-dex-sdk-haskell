{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Dex.Models where

import qualified PlutusTx.Builtins   as Builtins
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.TxId
import           Plutus.V1.Ledger.Scripts
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import           Servant.API
import Data.GADT.Compare
import Data.GADT.Show.TH
import Data.Aeson.GADT.TH

newtype PoolId = PoolId Builtins.ByteString
    deriving (Show, Generic, FromJSON, ToJSON, Eq, FromHttpApiData)

newtype GId = GId { gIdx :: Integer }
    deriving (Show, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut {
    txOutRefId       :: TxId,
    txOutRefIdx      :: Integer, -- ^ Index into the referenced transaction's outputs
    txOutAddress     :: Address,
    txOutValue       :: Value,
    fullTxOutDatum   :: Datum
} deriving (Show, Generic, FromJSON, ToJSON)

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

deriveJSONGADT ''Operation

data ParsedOperation = forall a. ParsedOperation { op :: Operation a }

data PoolData = PoolData {
    poolId :: PoolId,
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