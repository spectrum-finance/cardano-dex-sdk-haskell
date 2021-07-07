{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Dex.Processor where

import           Dex.Models
import           Dex.Contract.Models
import           Proxy.Contract.OnChain
import           Data.Maybe
import           PlutusTx.IsData                      (IsData (..))
import qualified Ledger.Typed.Scripts                 as Scripts
import           Data.Singletons.Prelude
import qualified PlutusTx
import qualified Ledger
import           Proxy.Contract.Models
import           Plutus.V1.Ledger.Value as Value

getPoolOperation :: FullTxOut -> Maybe ParsedOperation
getPoolOperation txOut = fmap (produceOperation txOut) (getAcceptableDatum txOut)

getPool :: FullTxOut -> Maybe Pool
getPool txOut = fmap producePool (getAcceptableDatum txOut)

getAcceptableDatum :: (IsData a) => FullTxOut -> Maybe a
getAcceptableDatum txOut =
    let generalDatum = fullTxOutDatum txOut
        datumData = Ledger.getDatum generalDatum
    in PlutusTx.fromData datumData

produceOperation :: FullTxOut -> ProxyDatum -> ParsedOperation
produceOperation txOut proxyDatum =
    case (action proxyDatum) of
        Swap -> let
            swapOpData = produceSwapOpData txOut proxyDatum
            in ParsedOperation (SwapOperation swapOpData)
        Return -> let
            redeemOpData = produceRedeemOpData txOut proxyDatum
            in ParsedOperation (RedeemOperation redeemOpData)

producePool :: ErgoDexPool -> Pool
producePool txOut = undefined

--todo: check
produceSwapOpData :: FullTxOut -> ProxyDatum -> SwapOpData
produceSwapOpData fulltxOut ProxyDatum{..} =
    let
        currentOutputValue = valueOf (txOutValue fulltxOut) (CurrencySymbol $ fromCurSymbol) (TokenName $ fromTokenName)
        minValue = (currentOutputValue * rate) * (100 - slippageTolerance)
    in SwapOpData {
        swapPoolId = PoolId targetPoolId,
        inputTokenSymbol = fromCurSymbol,
        inputTokenName = fromTokenName,
        minOutputTokenValue = minValue,
        dexFee = dexFeeDatum,
        userPubKey = userPubKey,
        proxyBox = fulltxOut
       }

produceRedeemOpData :: FullTxOut -> ProxyDatum -> RedeemOpData
produceRedeemOpData fulltxOut ProxyDatum{..} =
    RedeemOpData {
        redeemPoolId = PoolId targetPoolId,
        lpTokenSymbol = lpTokenSymbol,
        lpTokenName = lpTokenName,
        dexFee = dexFeeDatum,
        userPubKey = userPubKey,
        proxyBox = fulltxOut
    }

--todo: rename toSymbol, toTokenName because it is not real returned token
produceDepositOpData :: FullTxOut -> ProxyDatum -> DepositOpData
produceDepositOpData fulltxOut ProxyDatum{..} =
    DepositOpData {
        depositPoolId = PoolId targetPoolId,
        inputTokenXSymbol = fromCurSymbol,
        inputTokenXName = fromTokenName,
        inputTokenYSymbol = toSymbol,
        inputTokenYName = toTokenName,
        dexFee = dexFeeDatum,
        userPubKey = userPubKey,
        proxyBox = fulltxOut
    }