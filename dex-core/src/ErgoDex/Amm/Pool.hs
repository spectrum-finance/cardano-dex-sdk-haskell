module ErgoDex.Amm.Pool where

import Data.Bifunctor
import Data.Aeson             (FromJSON, ToJSON)
import GHC.Generics           (Generic)
import Data.Functor           ((<&>))
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO)

import Ledger
import Ledger.Value                    (assetClassValue, assetClassValueOf)
import PlutusTx.IsData.Class
import PlutusTx.Sqrt
import PlutusTx.Numeric                (AdditiveMonoid(zero))
import Ledger.Ada                      (lovelaceValueOf)

import CardanoTx.Models
    ( FullTxOut(FullTxOut, fullTxOutDatum, fullTxOutValue,
                fullTxOutAddress, fullTxOutRef),
      TxOutCandidate(TxOutCandidate, txOutCandidateAddress,
                     txOutCandidateValue, txOutCandidateDatum),
      TxOutDatum(KnownDatum) )
import           ErgoDex.Class
import           ErgoDex.Types
import           ErgoDex.State
import qualified ErgoDex.Contracts.Typed       as S
import           ErgoDex.Contracts.Types
import qualified ErgoDex.Contracts.Proxy.Order as W
import           ErgoDex.Contracts.Pool
import           ErgoDex.Amm.Constants         (minSafeOutputAmount)
import           ErgoDex.Amm.Scripts           (poolAddress)

newtype PoolId = PoolId { unPoolId :: Coin Nft }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data PoolFee = PoolFee
  { poolFeeNum' :: Integer
  , poolFeeDen' :: Integer
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
  , outCollateral :: Amount Lovelace
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

feeDen :: Integer
feeDen = 1000

instance FromLedger Pool where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(KnownDatum (Datum d)), ..} = --todo add also check for address
    case fromBuiltinData d of
      (Just PoolConfig{..}) -> do
        let
          rx         = Amount $ assetClassValueOf fullTxOutValue poolX
          ry         = Amount $ assetClassValueOf fullTxOutValue poolY
          rlq        = Amount $ assetClassValueOf fullTxOutValue poolLq
          nft        = Amount $ assetClassValueOf fullTxOutValue poolNft
          lq         = maxLqCapAmount - rlq -- actual LQ emission
          collateral = if W.isAda poolX || W.isAda poolY then zero else minSafeOutputAmount
        when (rx == 0 || ry == 0 || rlq == 0 || nft /= 1) Nothing
        Just $ OnChain fout Pool
          { poolId        = PoolId $ Coin  poolNft
          , poolReservesX = rx
          , poolReservesY = ry
          , poolLiquidity = lq
          , poolCoinX     = Coin poolX
          , poolCoinY     = Coin poolY
          , poolCoinLq    = Coin poolLq
          , poolFee       = PoolFee poolFeeNum feeDen
          , outCollateral = collateral
          }
      _ -> Nothing
  parseFromLedger _ = Nothing

instance (MonadIO m) => ToLedger m Pool where
  toLedger Pool{..} = do
    address <- poolAddress
    pure $ TxOutCandidate
        { txOutCandidateAddress = address
        , txOutCandidateValue   = poolValue
        , txOutCandidateDatum   = KnownDatum $ Datum $ toBuiltinData poolConf
        }
    where
      nft            = unPoolId poolId
      poolLqReserves = maxLqCapAmount - poolLiquidity
      poolValue      = assetClassValue (unCoin nft) 1 <>
                       assetAmountValue (AssetAmount poolCoinLq poolLqReserves) <>
                       assetAmountValue (AssetAmount poolCoinX poolReservesX) <>
                       assetAmountValue (AssetAmount poolCoinY poolReservesY) <>
                       lovelaceValueOf (unAmount outCollateral)

      poolConf = PoolConfig
        { poolNft    = unCoin nft
        , poolX      = unCoin poolCoinX
        , poolY      = unCoin poolCoinY
        , poolLq     = unCoin poolCoinLq
        , poolFeeNum = poolFeeNum' poolFee
        }

data PoolInitError
  = InvalidLiquidity Integer
  | InsufficientInitialLiqudity (Amount Liquidity)
  deriving (Show, Eq)

initPool :: (MonadIO m) => S.PoolConfig -> Amount Liquidity -> (Amount X, Amount Y) -> m (Either PoolInitError (Predicted Pool, Amount Liquidity))
initPool cfg burnLq amounts =
  (\pool@Pool{..} -> toLedger pool <&> (\output -> (Predicted output pool, poolLiquidity))) `traverse` initPool' cfg burnLq amounts

initPool' :: S.PoolConfig -> Amount Liquidity -> (Amount X, Amount Y) -> Either PoolInitError Pool
initPool' S.PoolConfig{..} burnLq (inX, inY) = do
  unlockedLq <- initialLiquidityAmount poolLq (inX, inY) <&> getAmount
  when (unlockedLq <= burnLq) (Left $ InsufficientInitialLiqudity unlockedLq)
  let
    releasedLq    = unlockedLq - burnLq
    outCollateral =
      if isAda poolX || isAda poolY
        then zero
        else minSafeOutputAmount
  pure $ Pool
      { poolId        = PoolId poolNft
      , poolReservesX = inX
      , poolReservesY = inY
      , poolLiquidity = releasedLq
      , poolCoinX     = poolX
      , poolCoinY     = poolY
      , poolCoinLq    = poolLq
      , poolFee       = PoolFee poolFeeNum feeDen
      , outCollateral = outCollateral
      }


applyDeposit :: (MonadIO m) => Pool -> (Amount X, Amount Y) -> m (Predicted Pool)
applyDeposit p@Pool{..} (inX, inY) =
    toLedger nextPool <&> (`Predicted` nextPool)
  where
    (unlockedLq, (changeX, changeY)) = rewardLp p (inX, inY)

    nextPool = p
      { poolReservesX = poolReservesX + inX - changeX
      , poolReservesY = poolReservesY + inY - changeY
      , poolLiquidity = poolLiquidity + unlockedLq
      }

rewardLp :: Pool -> (Amount X, Amount Y) -> (Amount Liquidity, (Amount X, Amount Y))
rewardLp p@Pool{poolLiquidity=(Amount lq), poolReservesX=(Amount poolX), poolReservesY=(Amount poolY)} (Amount inX, Amount inY) =
    (unlockedLq, change)
  where
    minByX = inX * lq `div` poolX
    minByY = inY * lq `div` poolY
    change =
      if (minByX == minByY)
      then (Amount 0, Amount 0) -- empty change
      else
        if (minByX < minByY)
        then (Amount 0, Amount $ (minByY - minByX) * poolY `div` lq)
        else (Amount $ (minByX - minByY) * poolX `div` lq, Amount 0)
    unlockedLq = Amount (min minByX minByY)

applyRedeem :: (MonadIO m) => Pool -> Amount Liquidity -> m (Predicted Pool)
applyRedeem p@Pool{..} burnedLq =
    toLedger nextPool <&> (`Predicted` nextPool)
  where
    (outX, outY) = bimap getAmount getAmount (sharesAmount p burnedLq)

    nextPool = p
      { poolReservesX = poolReservesX - outX
      , poolReservesY = poolReservesY - outY
      , poolLiquidity = poolLiquidity - burnedLq
      }

applySwap :: (MonadIO m) => Pool -> AssetAmount Base -> m (Predicted Pool)
applySwap p@Pool{..} base =
  toLedger nextPool <&> (`Predicted` nextPool)
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

initialLiquidityAmount :: Coin Liquidity -> (Amount X, Amount Y) -> Either PoolInitError (AssetAmount Liquidity)
initialLiquidityAmount poolCoinLq (Amount inX, Amount inY) =
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
        ((poolReservesY' * baseAmount * poolFeeNum') `div` (poolReservesX' * poolFeeDen' + baseAmount * poolFeeNum'))
    else
      assetAmountCoinOf (retagCoin poolCoinX)
        ((poolReservesX' * baseAmount * poolFeeNum') `div` (poolReservesY' * poolFeeDen' + baseAmount * poolFeeNum'))
  where
    xy             = unCoin baseAsset == unCoin poolCoinX
    poolReservesX' = unAmount poolReservesX
    poolReservesY' = unAmount poolReservesY
