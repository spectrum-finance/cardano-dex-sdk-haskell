module ErgoDex.Amm.Orders where

import Ledger

import ErgoDex.Types
import ErgoDex.Amm.Types
import ErgoDex.Contracts.Types

data Swap = Swap
  { swapBaseIn      :: Amount Base
  , swapMinQuoteOut :: Amount Quote
  , swapExFee       :: ExFeePerToken
  , swapRewardPkh   :: PubKeyHash
  } deriving (Show, Eq)

data Deposit = Deposit
  { depositX         :: Amount X
  , depositY         :: Amount Y
  , depositExFee     :: ExFee
  , depositRewardPkh :: PubKeyHash
  } deriving (Show, Eq)

data Redeem = Redeem
  { redeemLq        :: Amount Liquidity
  , redeemExFee     :: ExFee
  , redeemRewardPkh :: PubKeyHash
  } deriving (Show, Eq)

data OrderAction a where
  SwapAction    :: Swap -> OrderAction Swap
  DepositAction :: Deposit -> OrderAction Deposit
  RedeemAction  :: Redeem -> OrderAction Redeem

instance Show (OrderAction a) where
  show (SwapAction swap)       = show swap
  show (DepositAction deposit) = show deposit
  show (RedeemAction redeem)   = show redeem

instance Eq (OrderAction a) where
  (SwapAction x) == (SwapAction y)       = x == y
  (DepositAction x) == (DepositAction y) = x == y
  (RedeemAction x) == (RedeemAction y)   = x == y

data Order a = Order { orderPoolId :: PoolId, orderAction :: OrderAction a }
  deriving (Show, Eq)
