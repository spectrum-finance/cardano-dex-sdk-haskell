{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Dex.Processor
    ( ProcessorService(..)
    , mkProcessorService
    ) where

import           Dex.Models
import           Dex.Contract.Models
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Crypto
import           Plutus.V1.Ledger.Tx                  (TxOutRef (..))
import           Plutus.V1.Ledger.Value
import           PlutusTx.IsData                      (IsData (..))
import qualified PlutusTx
import qualified Ledger
import           Proxy.Contract.Models
import           Plutus.V1.Ledger.Value as Value
import           Utils                                (getPoolId, PoolId (..))

data ProcessorService = ProcessorService
    { getPoolOperation :: FullTxOut -> Maybe ParsedOperation
    , getPool :: FullTxOut -> Maybe Pool
    }

mkProcessorService :: ProcessorService
mkProcessorService = ProcessorService getPoolOperation' getPool'

getPoolOperation' :: FullTxOut -> Maybe ParsedOperation
getPoolOperation' txOut = fmap (produceOperation' txOut) (getAcceptableDatum txOut)

getPool' :: FullTxOut -> Maybe Pool
getPool' txOut = fmap (producePool txOut) (getAcceptableDatum txOut)

getAcceptableDatum :: (IsData a) => FullTxOut -> Maybe a
getAcceptableDatum txOut =
    let generalDatum = fullTxOutDatum txOut
        datumData = Ledger.getDatum generalDatum
    in PlutusTx.fromData datumData

produceOperation' :: FullTxOut -> ProxyDatum -> ParsedOperation
produceOperation' txOut proxyDatum =
    case (action proxyDatum) of
        Swap -> let
            swapOpData = produceSwapOpData' txOut proxyDatum
            in ParsedOperation (SwapOperation swapOpData)
        Redeem -> let
            redeemOpData = produceRedeemOpData' txOut proxyDatum
            in ParsedOperation (RedeemOperation redeemOpData)

producePool :: FullTxOut -> ErgoDexPool -> Pool
producePool txOut pool =
    Pool {
        poolData = PoolData {
            poolId = getPoolId pool ,
            poolFee = feeNum pool,
            xPoolCoin = xCoin pool,
            yPoolCoin = yCoin pool,
            lpPoolCoin = lpCoin pool
        },
        fullTxOut = txOut,
        poolTxIn = TxOutRef {
            txOutRefId = refId txOut,
            txOutRefIdx = refIdx txOut
        }
    }

--todo: check
produceSwapOpData' :: FullTxOut -> ProxyDatum -> SwapOpData
produceSwapOpData' fulltxOut ProxyDatum{..} =
    let
        currentOutputValue = assetClassValueOf (txOutValue fulltxOut) xProxyToken
    in SwapOpData {
        swapPoolId = PoolId targetPoolId,
        toSwapCoin = xProxyToken,
        toGetCoin = yProxyToken,
        minOutputTokenValue = minOutputValue,
        dexFee = dexFeeDatum,
        userAddress = pubKeyHashAddress $ PubKeyHash userPubKeyHash,
        proxyBox = fulltxOut
       }

produceRedeemOpData' :: FullTxOut -> ProxyDatum -> RedeemOpData
produceRedeemOpData' fulltxOut ProxyDatum{..} =
    RedeemOpData {
        redeemPoolId = PoolId targetPoolId,
        redeemLpCoin = lpProxyToken,
        dexFee = dexFeeDatum,
        userAddress = pubKeyHashAddress $ PubKeyHash userPubKeyHash,
        proxyBox = fulltxOut
    }

--todo: rename toSymbol, toTokenName because it is not real returned token
produceDepositOpData' :: FullTxOut -> ProxyDatum -> DepositOpData
produceDepositOpData' fulltxOut ProxyDatum{..} =
    DepositOpData {
        depositPoolId = PoolId targetPoolId,
        depositXCoin = xProxyToken,
        depositYCoin = yProxyToken,
        dexFee = dexFeeDatum,
        userAddress = pubKeyHashAddress $ PubKeyHash userPubKeyHash,
        proxyBox = fulltxOut
    }