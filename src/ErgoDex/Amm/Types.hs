module ErgoDex.Amm.Types where

import Prelude
import ErgoDex.Contracts.Types
import Playground.Contract (FromJSON, ToJSON, Generic)

newtype PoolId = PoolId { unPoolId :: Coin Nft } deriving (Show, Eq, Generic, FromJSON, ToJSON)
