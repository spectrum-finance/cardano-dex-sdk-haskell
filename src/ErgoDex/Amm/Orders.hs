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

data OrderAction a = SwapAction Swap | DepositAction Deposit | RedeemAction Redeem
  deriving (Show, Eq)

data Order a = Order { orderPoolId :: PoolId, orderAction :: OrderAction a }
  deriving (Show, Eq)
