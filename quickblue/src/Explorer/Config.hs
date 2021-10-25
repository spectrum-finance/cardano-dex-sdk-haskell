module Explorer.Config where

import GHC.Natural
import GHC.Generics

data ExplorerConfig = ExplorerConfig
  { explorerHost :: String
  , explorerPort :: Natural
  } deriving (Generic, Show)
