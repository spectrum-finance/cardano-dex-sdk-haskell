module ErgoDex.Types where

import           Prelude
import           ErgoDex.Contracts.Types

data Lovelace = Lovelace
  deriving (Show, Eq)

data ExFeePerToken = ExFeePerToken
  { exFeePerTokenNum :: Integer
  , exFeePerTokenDen :: Integer
  } deriving (Show, Eq)

newtype ExFee = ExFee { unExFee :: Amount Lovelace }
  deriving (Show, Eq)
