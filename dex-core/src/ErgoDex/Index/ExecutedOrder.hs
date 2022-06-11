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
  { config       :: Swap
  , actualQuote  :: Amount Quote
  , orderInputId :: String
  , userOutputId :: String
  , poolOutputId :: String
  , poolInputId  :: String
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
        { config       = swap
        , actualQuote  = quote
        , orderInputId = show P.$ fullTxOutRef swapOut
        , userOutputId = show P.$ fullTxOutRef userOutput
        , poolOutputId = show P.$ fullTxOutRef currPool
        , poolInputId  = show P.$ fullTxOutRef prevPool
        }

data ExecutedDeposit = ExecutedDeposit
  { config       :: Deposit
  , rewardLq     :: AssetAmount Liquidity
  , orderInputId :: String
  , userOutputId :: String
  , poolOutputId :: String
  , poolInputId  :: String
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
        { config       = cfg
        , rewardLq     = lqReward
        , orderInputId = show P.$ fullTxOutRef depositOut
        , userOutputId = show P.$ fullTxOutRef userOutput
        , poolOutputId = show P.$ fullTxOutRef currPool
        , poolInputId  = show P.$ fullTxOutRef prevPool
        }

data ExecutedRedeem = ExecutedRedeem
  { config       :: Redeem
  , rewardX      :: AssetAmount X
  , rewardY      :: AssetAmount Y
  , orderInputId :: String
  , userOutputId :: String
  , poolOutputId :: String
  , poolInputId  :: String
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
        { config       = cfg
        , rewardX      = assetAmountX
        , rewardY      = assetAmountY
        , orderInputId = show P.$ fullTxOutRef redeemOut
        , userOutputId = show P.$ fullTxOutRef userOutput
        , poolOutputId = show P.$ fullTxOutRef currPool
        , poolInputId  = show P.$ fullTxOutRef prevPool
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
