module ErgoDex.Amm.Pool where

import ErgoDex.Types
import ErgoDex.Amm.Types
import ErgoDex.Contracts.Types

data PoolFee = PoolFee
  { poolFeeNum :: Integer
  , poolFeeDen :: Integer
  } deriving (Show, Eq)

data Pool = Pool
  { poolId        :: PoolId
  , poolX         :: Amount X
  , poolY         :: Amount Y
  , poolLiquidity :: Amount Liquidity
  , poolCoinX     :: Coin X
  , poolCoinY     :: Coin Y
  , poolFee       :: PoolFee
  } deriving (Show, Eq)

deposit :: Pool -> Amount X -> Amount Y -> Predicted Pool
deposit p@Pool{..} inX inY =
    Predicted $ p { poolX = poolX + inX, poolY = poolY + inY, poolLiquidity = poolLiquidity + unlockedLq }
  where
    inX'           = unAmount inX
    inY'           = unAmount inY
    poolX'         = unAmount poolX
    poolY'         = unAmount poolY
    poolLiquidity' = unAmount poolLiquidity
    unlockedLq     = Amount $ min (inX' * poolLiquidity' `div` poolX') (inY' * poolLiquidity' `div` poolY')

redeem :: Pool -> Amount Liquidity -> Predicted Pool
redeem p@Pool{..} burnedLq =
    Predicted $ p { poolX = poolX - outX, poolY = poolY - outY, poolLiquidity = poolLiquidity - burnedLq }
  where
    poolX'         = unAmount poolX
    poolY'         = unAmount poolY
    burnedLq'      = unAmount burnedLq
    poolLiquidity' = unAmount poolLiquidity
    outX           = Amount $ burnedLq' * poolX' `div` poolLiquidity'
    outY           = Amount $ burnedLq' * poolY' `div` poolLiquidity'

swap :: Pool -> AssetAmount Base -> Predicted Pool
swap p@Pool{poolFee=PoolFee{..}, ..} base =
  Predicted $
    if xy then
      p { poolX = Amount $ poolX' + baseAmount, poolY = Amount $ poolY' - quoteAmount }
    else
      p { poolX = Amount $ poolX' - quoteAmount, poolY = Amount $ poolY' + baseAmount }
  where
    xy          = (unCoin $ getAsset base) == (unCoin poolCoinX)
    baseAmount  = unAmount $ getAmount base
    poolX'      = unAmount poolX
    poolY'      = unAmount poolY
    quoteAmount =
      if xy then
        (poolY' * baseAmount * poolFeeNum) `div` (poolX' * poolFeeDen + baseAmount * poolFeeNum)
      else
        (poolX' * baseAmount * poolFeeNum) `div` (poolY' * poolFeeDen + baseAmount * poolFeeNum)
