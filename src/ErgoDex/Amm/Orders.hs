{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE ExistentialQuantification   #-}

module ErgoDex.Amm.Orders where

import Data.Tuple.Extra
import Data.Bifunctor

import           Ledger
import           PlutusTx.IsData.Class
import           Ledger.Value           (AssetClass(..), assetClassValueOf, flattenValue)
import qualified Ledger.Ada             as Ada

import Cardano.Models
import ErgoDex.Types
import ErgoDex.Class
import ErgoDex.State
import ErgoDex.Amm.Pool                 (PoolId(..))
import ErgoDex.Contracts.Types
import ErgoDex.Contracts.Proxy.Swap
import ErgoDex.Contracts.Proxy.Deposit
import ErgoDex.Contracts.Proxy.Redeem

data Swap = Swap
  { swapPoolId      :: PoolId
  , swapBaseIn      :: Amount Base
  , swapMinQuoteOut :: Amount Quote
  , swapBase        :: Coin Base
  , swapQuote       :: Coin Quote
  , swapExFee       :: ExFeePerToken
  , swapRewardPkh   :: PubKeyHash
  } deriving (Show, Eq)

instance FromLedger Swap where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just SwapDatum{..}) ->
          Just $ Confirmed fout Swap
            { swapPoolId      = PoolId poolNft
            , swapBaseIn      = baseIn
            , swapMinQuoteOut = minQuoteAmount
            , swapBase        = base
            , swapQuote       = quote
            , swapExFee       = ExFeePerToken exFeePerTokenNum exFeePerTokenDen
            , swapRewardPkh   = rewardPkh
            }
        where
          baseIn = Amount $ assetClassValueOf fullTxOutValue (unCoin base)
      _ -> Nothing
  parseFromLedger _ = Nothing

data Deposit = Deposit
  { depositPoolId    :: PoolId
  , depositPair      :: (AssetEntry, AssetEntry)
  , depositExFee     :: ExFee
  , depositRewardPkh :: PubKeyHash
  } deriving (Show, Eq)

instance FromLedger Deposit where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just DepositDatum{..}) ->
        case excludeAdaFlattened fullTxOutValue of
          [assetX, assetY] ->
              Just $ Confirmed fout Deposit
                { depositPoolId    = PoolId poolNft
                , depositPair      = pair
                , depositExFee     = ExFee $ Amount exFee
                , depositRewardPkh = rewardPkh
                }
            where
              toEntry = uncurry3 assetEntry
              pair    = bimap toEntry toEntry (assetX, assetY)
          _ -> Nothing
      _ -> Nothing
  parseFromLedger _ = Nothing

data Redeem = Redeem
  { redeemPoolId    :: PoolId
  , redeemLqIn      :: Amount Liquidity
  , redeemLq        :: Coin Liquidity
  , redeemExFee     :: ExFee
  , redeemRewardPkh :: PubKeyHash
  } deriving (Show, Eq)

instance FromLedger Redeem where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just RedeemDatum{..}) ->
        case excludeAdaFlattened fullTxOutValue of
          [(ac, tn, v)] ->
              Just $ Confirmed fout Redeem
                { redeemPoolId    = PoolId poolNft
                , redeemLqIn      = Amount v
                , redeemLq        = Coin $ AssetClass (ac, tn)
                , redeemExFee     = ExFee $ Amount exFee
                , redeemRewardPkh = rewardPkh
                }
          _ -> Nothing
      _ -> Nothing
  parseFromLedger _ = Nothing

-- Flatten value filtering out ADA along the way.
excludeAdaFlattened :: Value -> [(CurrencySymbol, TokenName, Integer)]
excludeAdaFlattened v = filter (\(s, _, _) -> s /= Ada.adaSymbol) (flattenValue v)

data OrderAction a where
  SwapAction    :: Swap    -> OrderAction Swap
  DepositAction :: Deposit -> OrderAction Deposit
  RedeemAction  :: Redeem  -> OrderAction Redeem

instance Show (OrderAction a) where
  show (SwapAction applySwap)       = show applySwap
  show (DepositAction applyDeposit) = show applyDeposit
  show (RedeemAction applyRedeem)   = show applyRedeem

instance Eq (OrderAction a) where
  (SwapAction x) == (SwapAction y)       = x == y
  (DepositAction x) == (DepositAction y) = x == y
  (RedeemAction x) == (RedeemAction y)   = x == y

data Order a = Order
  { orderPoolId :: PoolId
  , orderAction :: OrderAction a
  } deriving (Show, Eq)

data AnyOrder = forall a . (Show a) => AnyOrder
  { anyOrderPoolId :: PoolId
  , anyOrderAction :: OrderAction a
  }

deriving instance Show AnyOrder