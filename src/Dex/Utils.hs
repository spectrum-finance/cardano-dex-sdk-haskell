{-# LANGUAGE RecordWildCards #-}

module Dex.Utils where

import           Dex.Models as ROp  (RedeemOpData(..))
import           Dex.Models as DOp  (DepositOpData(..))
import           Dex.Models as SOp  (SwapOpData(..))
import           Dex.Models
import           Ledger                           hiding (txOutValue)
import qualified PlutusTx.Builtins                as Builtins
import           Plutus.V1.Ledger.Value as Value
import qualified PlutusTx.AssocMap                as Map
import           Plutus.V1.Ledger.Address
import           Dex.Contract.Models
import           Utils

fullTxOut2TxOut :: FullTxOut -> TxOut
fullTxOut2TxOut _ = undefined

generateEmptyValue :: Value
generateEmptyValue = Value Map.empty

feeDenominator :: Integer
feeDenominator = 1000

getPoolId :: ErgoDexPool -> PoolId
getPoolId pool = undefined

totalEmissionLP :: Integer
totalEmissionLP = 0x7fffffffffffffff

sharesLP :: RedeemOpData -> Pool -> (Integer, Integer)
sharesLP RedeemOpData{..} Pool{..} =
    let poolValue = txOutValue fullTxOut
        proxyBoxValue = txOutValue proxyBox
        lpQtyInPool = assetClassValueOf poolValue (lpPoolCoin poolData)
        lpQtyInBox = assetClassValueOf proxyBoxValue (lpPoolCoin poolData)
        xQtyInPool = assetClassValueOf poolValue (xPoolCoin poolData)
        yQtyInPool = assetClassValueOf poolValue (yPoolCoin poolData)
        supplyLP = totalEmissionLP - lpQtyInPool
        xValue = lpQtyInBox * xQtyInPool `div` supplyLP
        yValue = lpQtyInBox * yQtyInPool `div` supplyLP
    in (xValue, yValue)