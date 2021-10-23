module ErgoDex.Amm.Orders where

import           Data.Tuple.Extra
import           Data.Bifunctor
import Data.Aeson
import Data.Maybe

import GHC.Generics

import           Ledger
import           PlutusTx.IsData.Class
import qualified Ledger.Value           as LedgerV
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
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

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
          baseIn = Amount $ LedgerV.assetClassValueOf fullTxOutValue (unCoin base)
      _ -> Nothing
  parseFromLedger _ = Nothing

data Deposit = Deposit
  { depositPoolId    :: PoolId
  , depositPair      :: (AssetEntry, AssetEntry)
  , depositExFee     :: ExFee
  , depositRewardPkh :: PubKeyHash
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromLedger Deposit where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just DepositDatum{..}) ->
        case extractPairValue fullTxOutValue of
          [assetX, assetY] ->
              Just $ Confirmed fout Deposit
                { depositPoolId    = PoolId poolNft
                , depositPair      = pair
                , depositExFee     = ExFee exFee
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
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromLedger Redeem where
  parseFromLedger fout@FullTxOut{fullTxOutDatum=(Just (Datum d)), ..} =
    case fromBuiltinData d of
      (Just RedeemDatum{..}) ->
        case extractPairValue fullTxOutValue of
          [(ac, tn, v)] ->
              Just $ Confirmed fout Redeem
                { redeemPoolId    = PoolId poolNft
                , redeemLqIn      = Amount v
                , redeemLq        = Coin $ LedgerV.AssetClass (ac, tn)
                , redeemExFee     = ExFee exFee
                , redeemRewardPkh = rewardPkh
                }
          _ -> Nothing
      _ -> Nothing
  parseFromLedger _ = Nothing

-- Extract pair of assets for order (Deposit|Redeem) from a given value.
extractPairValue :: LedgerV.Value -> [(CurrencySymbol, TokenName, Integer)]
extractPairValue inputValue =
    if (length flattenedInput == 2)
    then flattenedInput
    else filter (\(s, _, _) -> s /= Ada.adaSymbol) flattenedInput
  where
    flattenedInput = LedgerV.flattenValue inputValue

data OrderAction a where
  SwapAction    :: Swap    -> OrderAction Swap
  DepositAction :: Deposit -> OrderAction Deposit
  RedeemAction  :: Redeem  -> OrderAction Redeem

instance Show (OrderAction a) where
  show (SwapAction swap)       = show swap
  show (DepositAction deposit) = show deposit
  show (RedeemAction redeem)   = show redeem

instance Eq (OrderAction a) where
  (SwapAction x) == (SwapAction y)       = x == y
  (DepositAction x) == (DepositAction y) = x == y
  (RedeemAction x) == (RedeemAction y)   = x == y

instance ToJSON (OrderAction a) where
  toJSON (SwapAction swap)       = toJSON swap
  toJSON (DepositAction deposit) = toJSON deposit
  toJSON (RedeemAction redeem)   = toJSON redeem

  toEncoding (SwapAction swap)       = toEncoding swap
  toEncoding (DepositAction deposit) = toEncoding deposit
  toEncoding (RedeemAction redeem)   = toEncoding redeem

instance FromJSON (OrderAction a) where
  parseJSON v =
      if (isJust maybeSwap)         then return $ unsafeFromMaybe $ fmap SwapAction maybeSwap
      else if (isJust maybeDeposit) then return $ unsafeFromMaybe $ fmap DepositAction maybeDeposit
      else  return $ unsafeFromMaybe $ fmap RedeemAction maybeRedeem
      -- else                               return $ Nothing
    where
      maybeSwap    = unwrapResult ((fromJSON v) :: Result (Maybe Swap))
      maybeDeposit = unwrapResult ((fromJSON v) :: Result (Maybe Deposit))
      maybeRedeem  = unwrapResult ((fromJSON v) :: Result (Maybe Redeem))
    
unwrapResult :: Result (Maybe a) -> Maybe a
unwrapResult (Success a) = a
unwrapResult _           = Nothing

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe v =
    case v of
        Just v1 -> v1
        _ -> undefined

data Order a = Order
  { orderPoolId :: PoolId
  , orderAction :: OrderAction a
  } deriving (Show, Eq)

data AnyOrder = forall a . AnyOrder
  { anyOrderPoolId :: PoolId
  , anyOrderAction :: OrderAction a
  }

-- instance ToJSON AnyOrder where
--   ToJSON (AnyOrder unAnyOrderPoolId unAnyOrderAction) =
