module ErgoDex.Amm.Types where

import Prelude
import ErgoDex.Contracts.Types

newtype PoolId = PoolId { unPoolId :: Coin Nft } deriving (Show, Eq)
