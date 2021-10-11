module ErgoDex.Types where

import Prelude                 (Show, Eq, Integer)
import PlutusTx.Numeric        (AdditiveSemigroup(..), MultiplicativeSemigroup(..))
import ErgoDex.Contracts.Types

data Lovelace = Lovelace
  deriving (Show, Eq)

data AssetAmount a = AssetAmount
  { getAsset  :: Coin a
  , getAmount :: Amount a
  } deriving (Show, Eq)

instance AdditiveSemigroup (AssetAmount a) where
  a0 + a1 = a0 { getAmount = (getAmount a0) + (getAmount a1) }

instance MultiplicativeSemigroup (AssetAmount a) where
  a0 * a1 = a0 { getAmount = (getAmount a0) * (getAmount a1) }

data ExFeePerToken = ExFeePerToken
  { exFeePerTokenNum :: Integer
  , exFeePerTokenDen :: Integer
  } deriving (Show, Eq)

newtype ExFee = ExFee { unExFee :: Amount Lovelace }
  deriving (Show, Eq)

newtype Predicted a = Predicted { unPredicted :: a }
  deriving (Show, Eq)
