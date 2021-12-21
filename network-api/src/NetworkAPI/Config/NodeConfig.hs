module NetworkAPI.Config.NodeConfig where

import GHC.Natural

data NodeConfig = NodeConfig
  { port :: Natural
  , host :: String
  }