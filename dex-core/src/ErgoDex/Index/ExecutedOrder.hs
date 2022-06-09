module ErgoDex.Index.ExecutedOrder where

import ErgoDex.Amm.Orders
import ErgoDex.Contracts.Types as Currencies
import ErgoDex.State
import ErgoDex.Amm.Pool
import ErgoDex.Class
import ErgoDex.Types

import CardanoTx.Models

import           Data.Aeson (FromJSON, ToJSON)
import qualified PlutusTx.Prelude as P    
import           Ledger           
import           GHC.Generics                (Generic)

data ExecutedSwap = ExecutedSwap
  { swapCfg          :: Swap
  , actualQuote      :: Amount Quote
  , swapOrderInputId :: String
  , swapUserOutputId :: String
  , currPool         :: String
  , prevPoolId       :: String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer CompletedTx ExecutedSwap where
  parseFromExplorer CompletedTx{..} = do
    (OnChain swapOut swap@Swap{..}) <- findInput inputs :: Maybe (OnChain Swap)
    (OnChain prevPool _)            <- findInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)            <- findOutput outputs :: Maybe (OnChain Pool)
    userOutput                      <- findUserOutput swapRewardPkh swapRewardSPkh outputs
    let
      quote = amountOf (fullTxOutValue userOutput) swapQuote
    return 
      ExecutedSwap
        { swapCfg          = swap
        , actualQuote      = quote
        , swapOrderInputId = show P.$ fullTxOutRef swapOut
        , swapUserOutputId = show P.$ fullTxOutRef userOutput
        , currPool         = show P.$ fullTxOutRef currPool
        , prevPoolId       = show P.$ fullTxOutRef prevPool
        }

data ExecutedDeposit = ExecutedDeposit
  { depositCfg          :: Deposit
  , lqReward            :: AssetAmount Liquidity
  , depositOrderInputId :: String
  , depositUserOutputId :: String
  , currPool            :: String
  , prevPoolId          :: String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer CompletedTx ExecutedDeposit where
  parseFromExplorer CompletedTx{..} = do
    (OnChain depositOut cfg@Deposit{..}) <- findInput inputs :: Maybe (OnChain Deposit)
    (OnChain prevPool Pool{..})          <- findInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)                 <- findOutput outputs :: Maybe (OnChain Pool)
    userOutput                           <- findUserOutput depositRewardPkh depositRewardSPkh outputs
    let
      lqReward = assetAmountOfCoin (fullTxOutValue userOutput) poolCoinLq
    return 
      ExecutedDeposit
        { depositCfg          = cfg
        , lqReward            = lqReward
        , depositOrderInputId = show P.$ fullTxOutRef depositOut
        , depositUserOutputId = show P.$ fullTxOutRef userOutput
        , currPool            = show P.$ fullTxOutRef currPool
        , prevPoolId          = show P.$ fullTxOutRef prevPool
        }

data ExecutedRedeem = ExecutedRedeem
  { redeemCfg          :: Redeem
  , xReward            :: AssetAmount X
  , yReward            :: AssetAmount Y
  , redeemOrderInputId :: String
  , redeemUserOutputId :: String
  , currPool           :: String
  , prevPoolId         :: String
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromExplorer CompletedTx ExecutedRedeem where
  parseFromExplorer CompletedTx{..} = do
    (OnChain redeemOut cfg@Redeem{..}) <- findInput inputs :: Maybe (OnChain Redeem)
    (OnChain prevPool Pool{..})        <- findInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)               <- findOutput outputs :: Maybe (OnChain Pool)
    userOutput                         <- findUserOutput redeemRewardPkh redeemRewardSPkh outputs
    let
      assetAmountX = assetAmountOfCoin (fullTxOutValue userOutput) poolCoinX
      assetAmountY = assetAmountOfCoin (fullTxOutValue userOutput) poolCoinY
    return 
      ExecutedRedeem
        { redeemCfg          = cfg
        , xReward            = assetAmountX
        , yReward            = assetAmountY
        , redeemOrderInputId = show P.$ fullTxOutRef redeemOut
        , redeemUserOutputId = show P.$ fullTxOutRef userOutput
        , currPool           = show P.$ fullTxOutRef currPool
        , prevPoolId         = show P.$ fullTxOutRef prevPool
        }

findInput :: forall a . (FromLedger a) => [FullTxIn] -> Maybe (OnChain a)
findInput (FullTxIn{..}:xs) =
  case parseFromLedger fullTxInTxOut of
      Just r -> Just r
      _      -> findInput xs
findInput [] = Nothing

findOutput :: forall a . (FromLedger a) => [FullTxOut] -> Maybe (OnChain a)
findOutput (x:xs) =
  case parseFromLedger x of
      Just r -> Just r
      _      -> findOutput xs
findOutput [] = Nothing

findUserOutput :: PubKeyHash -> Maybe StakePubKeyHash -> [FullTxOut] -> Maybe FullTxOut
findUserOutput key pkh (x@FullTxOut{..}:xs) =
    if (fullTxOutAddress P.== pubKeyHashAddress (PaymentPubKeyHash key) pkh) 
      then Just x 
      else (findUserOutput key pkh xs)
findUserOutput _ _ [] = Nothing
