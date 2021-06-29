{-# LANGUAGE FlexibleContexts       #-}

module Dex.Processor where

import           Dex.Models
import           Dex.Types
import           Proxy.Contract.OnChain
import           Data.Maybe
import           PlutusTx.IsData                      (IsData (..))
import qualified Ledger.Typed.Scripts                 as Scripts
import qualified PlutusTx
import           Ledger
import           Proxy.Contract.Types

getPoolOperation :: FullTxOut -> Maybe (Operation a)
getPoolOperation txOut = fmap produceOperation (getAcceptableDatum txOut)

getPool :: FullTxOut -> Maybe Pool
getPool txOut = fmap producePool (getAcceptableDatum txOut)

getAcceptableDatum :: (PlutusTx.IsData a) => FullTxOut -> Maybe a
getAcceptableDatum txOut =
    let generalDatum = fullTxOutDatum txOut
        datumData = getDatum generalDatum
    in PlutusTx.fromData datumData

produceOperation :: ProxyDatum -> Operation a
produceOperation proxyDatum = undefined

producePool :: ErgoDexPool -> Pool
producePool txOut = undefined