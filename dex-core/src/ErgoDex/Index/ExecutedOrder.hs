module ErgoDex.Index.ExecutedOrder where

import ErgoDex.Amm.Orders
import ErgoDex.Contracts.Types as Currencies
import Data.Text               as T
import           Ledger           
import           PlutusTx.Prelude as P    
import CardanoTx.Models
import ErgoDex.State
import ErgoDex.Class
import ErgoDex.Amm.Pool

import           Ledger          (Redeemer(..), PaymentPubKeyHash(..), pubKeyHashAddress)
-- data OrderType = Swap | Redeem | Deposit

class FromExplorer a b where
  parseFromExplorer :: a -> b

data ExecutedSwap = ExecutedSwap
  { swapCfg     :: Swap
  , actualQuote :: Amount Quote
  , swapOrderInputId    :: Text
  , swapUserOutputId    :: Text
  , poolAfterSwapId :: Text
  , prevPoolId :: Text
  }

-- class FromExplorer CompletedTx (Maybe ExecutedSwap) where
--   parseFromExplorer CompletedTx{..} =
--     let
--       maybeSwap = getOutputV outputs :: (Maybe (OnChain Swap))
--       maybePool = getOutputV outputs
--     in

fromExplorer :: CompletedTx -> () -- ExecutedSwap
fromExplorer CompletedTx{..} =
  let
      maybeSwap = getInputV inputs :: (Maybe (OnChain Swap)) -- we have to find it in inputs!
      maybeOutPool = getOutputV outputs :: (Maybe (OnChain Pool))
      maybeUserOutput =
        maybeSwap >>= (\(OnChain out Swap{..}) ->
            getUserOutput swapRewardPkh swapRewardSPkh outputs
          )
      maybeInPool = getInputV inputs :: (Maybe (OnChain Pool)) -- we have to find it in inputs!
  in ()

getInputV :: forall a . (FromLedger a) => [FullTxIn] -> Maybe (OnChain a)
getInputV (FullTxIn{..}:xs) =
  let
    maybeSwap = parseFromLedger fullTxInTxOut :: Maybe (OnChain a)
  in case maybeSwap of
      Just r -> Just r
      _ -> getInputV xs
getInputV [] = Nothing



getOutputV :: forall a . (FromLedger a) => [FullTxOut] -> Maybe (OnChain a)
getOutputV (x:xs) =
  let
    maybeSwap = parseFromLedger x :: Maybe (OnChain a)
  in case maybeSwap of
      Just r -> Just r
      _ -> getOutputV xs
getOutputV [] = Nothing

getUserOutput :: PubKeyHash -> Maybe StakePubKeyHash -> [FullTxOut] -> Maybe FullTxOut
getUserOutput key pkh (x@FullTxOut{..}:xs) =
    if (fullTxOutAddress P.== pubKeyHashAddress (PaymentPubKeyHash key) pkh) 
      then Just x 
      else (getUserOutput key pkh xs)
getUserOutput key pkh [] = Nothing