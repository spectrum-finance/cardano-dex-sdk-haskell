module Explorer.Config where

import GHC.Generics
import Dhall

newtype Uri = Uri { unUri :: String }
  deriving Generic
  deriving newtype (Show, FromDhall)

data Network = Mainnet | Preview
  deriving (Generic, Show, FromDhall)

data ExplorerConfig = ExplorerConfig
  { explorerUri :: Uri
  , network     :: Network
  } deriving (Generic, Show)

instance FromDhall ExplorerConfig