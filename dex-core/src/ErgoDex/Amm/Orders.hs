module ErgoDex.Amm.Orders where

import           Data.Tuple.Extra
import           Data.Bifunctor
import           Data.Function
import           Data.Aeson       (FromJSON(..), ToJSON(..), object, (.=), (.:))
import qualified Data.Aeson       as JSON
import           GHC.Generics     (Generic)

import           Ledger
import           PlutusTx.IsData.Class
import           Ledger.Value          (AssetClass(..), assetClassValueOf, flattenValue)
import qualified Ledger.Ada            as Ada

import CardanoTx.Models
import ErgoDex.Types
import ErgoDex.Class
import ErgoDex.State
import ErgoDex.Amm.Pool                (PoolId(..))
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
          baseIn = Amount $ assetClassValueOf fullTxOutValue (unCoin base)
      _ -> Nothing
  parseFromLedger _ = Nothing

data Deposit = Deposit
  { depositPoolId    :: PoolId
  , depositPair      :: (AssetEntry, AssetEntry)
  , depositExFee     :: ExFee
  , depositRewardPkh :: PubKeyHash
  , adaCollateral    :: Amount Lovelace
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
                , adaCollateral    = collateralAda
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
                , redeemLq        = Coin $ AssetClass (ac, tn)
                , redeemExFee     = ExFee exFee
                , redeemRewardPkh = rewardPkh
                }
          _ -> Nothing
      _ -> Nothing
  parseFromLedger _ = Nothing

-- Extract pair of assets for order (Deposit|Redeem) from a given value.
extractPairValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
extractPairValue inputValue =
    if length flattenedInput == 2
    then flattenedInput
    else filter (\(s, _, _) -> s /= Ada.adaSymbol) flattenedInput
  where
    flattenedInput = flattenValue inputValue

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

data Order a = Order
  { orderPoolId :: PoolId
  , orderAction :: OrderAction a
  } deriving (Show, Eq)

data AnyOrder = forall a . AnyOrder
  { anyOrderPoolId :: PoolId
  , anyOrderAction :: OrderAction a
  }

data ActionKind = SwapK | DepositK | RedeemK
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToJSON AnyOrder where
  toJSON (AnyOrder poolId (SwapAction swap)) =
    object [ "kind"   .= SwapK
           , "poolId" .= poolId
           , "action" .= toJSON swap
           ]
  toJSON (AnyOrder poolId (DepositAction deposit)) =
    object [ "kind"   .= DepositK
           , "poolId" .= poolId
           , "action" .= toJSON deposit
           ]
  toJSON (AnyOrder poolId (RedeemAction redeem)) =
    object [ "kind"   .= RedeemK
           , "poolId" .= poolId
           , "action" .= toJSON redeem
           ]

instance FromJSON AnyOrder where
  parseJSON (JSON.Object v) = do
    kind   <- v .: "kind"
    poolId <- v .: "poolId"

    let actionP = v .: "action"

    case kind of
      SwapK    -> actionP >>= JSON.parseJSON & fmap (AnyOrder poolId . SwapAction)
      DepositK -> actionP >>= JSON.parseJSON & fmap (AnyOrder poolId . DepositAction)
      RedeemK  -> actionP >>= JSON.parseJSON & fmap (AnyOrder poolId . RedeemAction)

  parseJSON _ = fail "expected an object"
