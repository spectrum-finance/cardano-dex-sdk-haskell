module ErgoDex.Amm.Pool where

import Data.Bifunctor

import Ledger
import Ledger.Value                    (assetClassValue, assetClassValueOf)
import Ledger.Typed.Scripts.Validators
import PlutusTx.IsData.Class
import PlutusTx.Sqrt
import Data.Aeson                      (FromJSON, ToJSON)
import GHC.Generics                    (Generic)

import CardanoTx.Models
import ErgoDex.Class
import ErgoDex.Types
import ErgoDex.State
import ErgoDex.Contracts.Types
import ErgoDex.Contracts.Pool
import ErgoDex.Contracts.OffChain
import ErgoDex.Contracts.Proxy.Order (isAda)
import ErgoDex.Amm.Constants (minSafeOutputValue)
import PlutusTx.Numeric (AdditiveMonoid(zero))
import Plutus.V1.Ledger.Ada (lovelaceValueOf)

newtype PoolId = PoolId { unPoolId :: Coin Nft }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

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
  , collateral    :: Amount Lovelace
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromLedger Pool where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just PoolDatum{..}) ->
          Just $ Confirmed fout Pool
            { poolId        = PoolId poolNft
            , poolReservesX = rx
            , poolReservesY = ry
            , poolLiquidity = lq
            , poolCoinX     = poolX
            , poolCoinY     = poolY
            , poolCoinLq    = poolLq
            , poolFee       = PoolFee feeNum feeDen
            , collateral    = collateral
            }
        where
          rx         = Amount $ assetClassValueOf fullTxOutValue (unCoin poolX)
          ry         = Amount $ assetClassValueOf fullTxOutValue (unCoin poolY)
          rlq        = Amount $ assetClassValueOf fullTxOutValue (unCoin poolLq)
          lq         = maxLqCap - rlq -- actual LQ emission
          collateral = if isAda poolX || isAda poolY then zero else minSafeOutputValue
          feeDen     = 1000
      _ -> Nothing
  parseFromLedger _ = Nothing

instance ToLedger Pool where
  toLedger Pool{..} =
      TxOutCandidate
        { txOutCandidateAddress  = validatorAddress poolInstance
        , txOutCandidateValue    = poolValue
        , txOutCandidateDatum    = Just $ Datum $ toBuiltinData nextPoolDatum
        }
    where
      nft            = unPoolId poolId
      poolLqReserves = maxLqCap - poolLiquidity
      poolValue      = assetClassValue (unCoin nft) 1 <>
                       assetAmountValue (AssetAmount poolCoinLq poolLqReserves) <>
                       assetAmountValue (AssetAmount poolCoinX poolReservesX) <>
                       assetAmountValue (AssetAmount poolCoinY poolReservesY) <>
                       lovelaceValueOf (unAmount collateral)

      nextPoolDatum = PoolDatum
        { poolNft = nft
        , poolX   = poolCoinX
        , poolY   = poolCoinY
        , poolLq  = poolCoinLq
        , feeNum  = poolFeeNum poolFee
        }

data PoolInitError = InvalidLiquidity Integer

applyInit :: Pool -> (Amount X, Amount Y) -> Either PoolInitError (Predicted Pool, Amount Liquidity)
applyInit p@Pool{..} (inX, inY) = do
  unlockedLq <- fmap getAmount (initialLiquidityAmount p (inX, inY))
  let
    nextPool = p
      { poolReservesX = poolReservesX + inX
      , poolReservesY = poolReservesY + inY
      , poolLiquidity = poolLiquidity + unlockedLq
      }
    nextPoolOut = toLedger nextPool
  Right (Predicted nextPoolOut nextPool, unlockedLq)

applyDeposit :: Pool -> (Amount X, Amount Y) -> Predicted Pool
applyDeposit p@Pool{..} (inX, inY) =
    Predicted nextPoolOut nextPool
  where
    unlockedLq = getAmount (liquidityAmount p (inX, inY))

    nextPool = p
      { poolReservesX = poolReservesX + inX
      , poolReservesY = poolReservesY + inY
      , poolLiquidity = poolLiquidity + unlockedLq
      }

    nextPoolOut = toLedger nextPool

applyRedeem :: Pool -> Amount Liquidity -> Predicted Pool
applyRedeem p@Pool{..} burnedLq =
    Predicted nextPoolOut nextPool
  where
    (outX, outY) = bimap getAmount getAmount (sharesAmount p burnedLq)

    nextPool = p
      { poolReservesX = poolReservesX - outX
      , poolReservesY = poolReservesY - outY
      , poolLiquidity = poolLiquidity - burnedLq
      }

    nextPoolOut = toLedger nextPool

applySwap :: Pool -> AssetAmount Base -> Predicted Pool
applySwap p@Pool{..} base =
  Predicted nextPoolOut nextPool
  where
    xy             = unCoin (getAsset base) == unCoin poolCoinX
    baseAmount     = unAmount $ getAmount base
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    quoteAmount    = assetAmountRawValue (outputAmount p base)

    nextPool =
      if xy then p
        { poolReservesX = Amount $ poolReservesX' + baseAmount
        , poolReservesY = Amount $ poolReservesY' - quoteAmount
        }
      else p
        { poolReservesX = Amount $ poolReservesX' - quoteAmount
        , poolReservesY = Amount $ poolReservesY' + baseAmount
        }

    nextPoolOut = toLedger nextPool

initialLiquidityAmount :: Pool -> (Amount X, Amount Y) -> Either PoolInitError (AssetAmount Liquidity)
initialLiquidityAmount Pool{..} (Amount inX, Amount inY) =
  fmap (assetAmountCoinOf poolCoinLq) $ case isqrt (inX * inY) of
    Exactly l | l > 0       -> Right l
    Approximately l | l > 0 -> Right $ l + 1
    _                       -> Left $ InvalidLiquidity (inX * inY)

liquidityAmount :: Pool -> (Amount X, Amount Y) -> AssetAmount Liquidity
liquidityAmount Pool{..} (Amount inX, Amount inY) =
    assetAmountCoinOf poolCoinLq $
      min (inX * poolLiquidity' `div` poolReservesX') (inY * poolLiquidity' `div` poolReservesY')
  where
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    poolLiquidity' = unAmount poolLiquidity

sharesAmount :: Pool -> Amount Liquidity -> (AssetAmount X, AssetAmount Y)
sharesAmount Pool{..} burnedLq =
    ( assetAmountCoinOf poolCoinX (burnedLq' * poolReservesX' `div` poolLiquidity')
    , assetAmountCoinOf poolCoinY (burnedLq' * poolReservesY' `div` poolLiquidity')
    )
  where
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
    burnedLq'      = unAmount burnedLq
    poolLiquidity' = unAmount poolLiquidity

outputAmount :: Pool -> AssetAmount Base -> AssetAmount Quote
outputAmount Pool{poolFee=PoolFee{..}, ..} (AssetAmount baseAsset (Amount baseAmount)) =
    if xy then
      assetAmountCoinOf (retagCoin poolCoinY)
        ((poolReservesY' * baseAmount * poolFeeNum) `div` (poolReservesX' * poolFeeDen + baseAmount * poolFeeNum))
    else
      assetAmountCoinOf (retagCoin poolCoinX)
        ((poolReservesX' * baseAmount * poolFeeNum) `div` (poolReservesY' * poolFeeDen + baseAmount * poolFeeNum))
  where
    xy             = unCoin baseAsset == unCoin poolCoinX
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
