{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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

getPoolOperation :: FullTxOut -> Maybe ParsedOperation
getPoolOperation txOut = fmap produceOperation (getAcceptableDatum txOut)

getPool :: FullTxOut -> Maybe Pool
getPool txOut = fmap producePool (getAcceptableDatum txOut)

getAcceptableDatum :: (IsData a) => FullTxOut -> Maybe a
getAcceptableDatum txOut =
    let generalDatum = fullTxOutDatum txOut
        datumData = Ledger.getDatum generalDatum
    in PlutusTx.fromData datumData

produceOperation :: ProxyDatum -> ParsedOperation
produceOperation proxyDatum =
    case (action proxyDatum) of
        Swap -> let
            swapOpData = produceSwapOpData proxyDatum
            in ParsedOperation (SwapOperation swapOpData)
        Return -> let
            redeemOpData = produceRedeemOpData proxyDatum
            in ParsedOperation (RedeemOperation redeemOpData)

producePool :: ErgoDexPool -> Pool
producePool txOut = undefined

produceSwapOpData :: ProxyDatum -> SwapOpData
produceSwapOpData _ = undefined

produceRedeemOpData :: ProxyDatum -> RedeemOpData
produceRedeemOpData _ = undefined

produceDepositOpData :: ProxyDatum -> DepositOpData
produceDepositOpData _ = undefined