module ErgoDex.Amm.Pool where

import Ledger
import Ledger.Value          (assetClassValueOf)
import PlutusTx.IsData.Class

import Cardano.Models
import Cardano.Class
import ErgoDex.Types
import ErgoDex.Contracts.Types
import ErgoDex.Contracts.Pool
import Playground.Contract (FromJSON, ToJSON, Generic)

newtype PoolId = PoolId { unPoolId :: Coin Nft }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PoolFee = PoolFee
  { poolFeeNum :: Integer
  , poolFeeDen :: Integer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Pool = Pool
  { poolId        :: PoolId
  , poolReservesX :: Amount X
  , poolReservesY :: Amount Y
  , poolLiquidity :: Amount Liquidity
  , poolCoinX     :: Coin X
  , poolCoinY     :: Coin Y
  , poolFee       :: PoolFee
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromLedger Pool where
  parseFromLedger FullTxOut{txOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just (PoolDatum PoolParams{..} lq)) ->
          Just $ Pool
            { poolId        = PoolId poolNft
            , poolReservesX = rx
            , poolReservesY = ry
            , poolLiquidity = lq
            , poolCoinX     = poolX
            , poolCoinY     = poolY
            , poolFee       = PoolFee feeNum feeDen
            }
        where
          rx     = Amount $ assetClassValueOf txOutValue (unCoin poolX)
          ry     = Amount $ assetClassValueOf txOutValue (unCoin poolY)
          feeDen = 1000
      _ -> Nothing
  parseFromLedger _ = Nothing

deposit :: Pool -> Amount X -> Amount Y -> Predicted Pool
deposit p@Pool{..} inX inY =
    Predicted $ p
      { poolReservesX = poolReservesX + inX
      , poolReservesY = poolReservesY + inY
      , poolLiquidity = poolLiquidity + unlockedLq
      }
  where
    inX'           = unAmount inX
    inY'           = unAmount inY
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    poolLiquidity' = unAmount poolLiquidity
    unlockedLq     = Amount $ min (inX' * poolLiquidity' `div` poolReservesX') (inY' * poolLiquidity' `div` poolReservesY')

redeem :: Pool -> Amount Liquidity -> Predicted Pool
redeem p@Pool{..} burnedLq =
    Predicted $ p
      { poolReservesX = poolReservesX - outX
      , poolReservesY = poolReservesY - outY
      , poolLiquidity = poolLiquidity - burnedLq
      }
  where
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    burnedLq'      = unAmount burnedLq
    poolLiquidity' = unAmount poolLiquidity
    outX           = Amount $ burnedLq' * poolReservesX' `div` poolLiquidity'
    outY           = Amount $ burnedLq' * poolReservesY' `div` poolLiquidity'

swap :: Pool -> AssetAmount Base -> Predicted Pool
swap p@Pool{poolFee=PoolFee{..}, ..} base =
  Predicted $
    if xy then p
      { poolReservesX = Amount $ poolReservesX' + baseAmount
      , poolReservesY = Amount $ poolReservesY' - quoteAmount
      }
    else p
      { poolReservesX = Amount $ poolReservesX' - quoteAmount
      , poolReservesY = Amount $ poolReservesY' + baseAmount
      }
  where
    xy             = (unCoin $ getAsset base) == (unCoin poolCoinX)
    baseAmount     = unAmount $ getAmount base
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    quoteAmount    =
      if xy then
        (poolReservesY' * baseAmount * poolFeeNum) `div` (poolReservesX' * poolFeeDen + baseAmount * poolFeeNum)
      else
        (poolReservesX' * baseAmount * poolFeeNum) `div` (poolReservesY' * poolFeeDen + baseAmount * poolFeeNum)
