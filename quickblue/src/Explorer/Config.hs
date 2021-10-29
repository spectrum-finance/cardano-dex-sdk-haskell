module Explorer.Config where

import GHC.Natural
import GHC.Generics
import Dhall

data ExplorerConfig = ExplorerConfig
  { explorerHost :: String
  , explorerPort :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerConfig