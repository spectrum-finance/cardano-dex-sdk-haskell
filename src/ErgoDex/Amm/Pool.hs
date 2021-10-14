module ErgoDex.Amm.Pool where

import Data.Bifunctor

import Ledger
import Ledger.Value          (assetClassValueOf)
import PlutusTx.IsData.Class

import Cardano.Models
import ErgoDex.Class
import ErgoDex.Types
import ErgoDex.State
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
  , poolCoinLq    :: Coin Liquidity
  , poolFee       :: PoolFee
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromLedger Pool where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just (PoolDatum PoolParams{..} lq)) ->
          Just $ Confirmed fout Pool
            { poolId        = PoolId poolNft
            , poolReservesX = rx
            , poolReservesY = ry
            , poolLiquidity = lq
            , poolCoinX     = poolX
            , poolCoinY     = poolY
            , poolCoinLq    = poolLq
            , poolFee       = PoolFee feeNum feeDen
            }
        where
          rx     = Amount $ assetClassValueOf fullTxOutValue (unCoin poolX)
          ry     = Amount $ assetClassValueOf fullTxOutValue (unCoin poolY)
          feeDen = 1000
      _ -> Nothing
  parseFromLedger _ = Nothing

applyDeposit :: Pool -> (Amount X, Amount Y) -> Predicted Pool
applyDeposit p@Pool{..} (inX, inY) =
    Predicted nextPoolOut p
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
    unlockedLq     = getAmount (liquidityAmount p (inX, inY))

    nextPoolOut = undefined

applyRedeem :: Pool -> Amount Liquidity -> Predicted Pool
applyRedeem p@Pool{..} burnedLq =
    Predicted nextPoolOut p
      { poolReservesX = poolReservesX - outX
      , poolReservesY = poolReservesY - outY
      , poolLiquidity = poolLiquidity - burnedLq
      }
  where
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    burnedLq'      = unAmount burnedLq
    poolLiquidity' = unAmount poolLiquidity
    (outX, outY)   = bimap getAmount getAmount (sharesAmount p burnedLq)

    nextPoolOut = undefined

applySwap :: Pool -> AssetAmount Base -> Predicted Pool
applySwap p@Pool{poolFee=PoolFee{..}, ..} base =
  Predicted nextPoolOut $
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
    quoteAmount    = assetAmountRawValue (outputAmount p base)

    nextPoolOut = undefined

liquidityAmount :: Pool -> (Amount X, Amount Y) -> AssetAmount Liquidity
liquidityAmount p@Pool{..} (inX, inY) =
    assetAmountCoinOf poolCoinLq $
      (min (inX' * poolLiquidity' `div` poolReservesX') (inY' * poolLiquidity' `div` poolReservesY'))
  where
    inX'           = unAmount inX
    inY'           = unAmount inY
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    poolLiquidity' = unAmount poolLiquidity

sharesAmount :: Pool -> Amount Liquidity -> (AssetAmount X, AssetAmount Y)
sharesAmount p@Pool{..} burnedLq =
    ( assetAmountCoinOf poolCoinX (burnedLq' * poolReservesX' `div` poolLiquidity')
    , assetAmountCoinOf poolCoinY (burnedLq' * poolReservesY' `div` poolLiquidity')
    )
  where
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    burnedLq'      = unAmount burnedLq
    poolLiquidity' = unAmount poolLiquidity

outputAmount :: Pool -> AssetAmount Base -> AssetAmount Quote
outputAmount p@Pool{poolFee=PoolFee{..}, ..} base =
    if xy then
      assetAmountCoinOf (retagCoin poolCoinY) $
        ((poolReservesY' * baseAmount * poolFeeNum) `div` (poolReservesX' * poolFeeDen + baseAmount * poolFeeNum))
    else
      assetAmountCoinOf (retagCoin poolCoinX) $
        ((poolReservesX' * baseAmount * poolFeeNum) `div` (poolReservesY' * poolFeeDen + baseAmount * poolFeeNum))
  where
    xy             = (unCoin $ getAsset base) == (unCoin poolCoinX)
    baseAmount     = assetAmountRawValue base
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
