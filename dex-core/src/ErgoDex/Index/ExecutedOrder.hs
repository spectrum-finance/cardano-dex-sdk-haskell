module ErgoDex.Index.ExecutedOrder where

import ErgoDex.Amm.Orders
import ErgoDex.Contracts.Types as Currencies
import ErgoDex.State
import ErgoDex.Amm.Pool
import ErgoDex.Class

import CardanoTx.Models

import qualified PlutusTx.Prelude as P    
import           Ledger           

data ExecutedSwap = ExecutedSwap
  { swapCfg          :: Swap
  , actualQuote      :: Amount Quote
  , swapOrderInputId :: String
  , swapUserOutputId :: String
  , currPool         :: String
  , prevPoolId       :: String
  }

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
  , depositOrderInputId :: String
  , depositUserOutputId :: String
  , currPool            :: String
  , prevPoolId          :: String
  }

instance FromExplorer CompletedTx ExecutedDeposit where
  parseFromExplorer CompletedTx{..} = do
    (OnChain depositOut cfg@Deposit{..}) <- findInput inputs :: Maybe (OnChain Deposit)
    (OnChain prevPool _)                 <- findInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)                 <- findOutput outputs :: Maybe (OnChain Pool)
    userOutput                           <- findUserOutput depositRewardPkh depositRewardSPkh outputs
    return 
      ExecutedDeposit
        { depositCfg          = cfg
        , depositOrderInputId = show P.$ fullTxOutRef depositOut
        , depositUserOutputId = show P.$ fullTxOutRef userOutput
        , currPool            = show P.$ fullTxOutRef currPool
        , prevPoolId          = show P.$ fullTxOutRef prevPool
        }

data ExecutedRedeem = ExecutedRedeem
  { redeemCfg          :: Redeem
  , redeemOrderInputId :: String
  , redeemUserOutputId :: String
  , currPool           :: String
  , prevPoolId         :: String
  }

instance FromExplorer CompletedTx ExecutedRedeem where
  parseFromExplorer CompletedTx{..} = do
    (OnChain redeemOut cfg@Redeem{..}) <- findInput inputs :: Maybe (OnChain Redeem)
    (OnChain prevPool _)               <- findInput inputs :: Maybe (OnChain Pool)
    (OnChain currPool _)               <- findOutput outputs :: Maybe (OnChain Pool)
    userOutput                         <- findUserOutput redeemRewardPkh redeemRewardSPkh outputs
    return 
      ExecutedRedeem
        { redeemCfg          = cfg
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
