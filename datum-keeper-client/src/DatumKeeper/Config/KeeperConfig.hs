module DatumKeeper.Config.KeeperConfig where

import GHC.Natural

data DatumKeeperConfig = DatumKeeperConfig
  { host         :: String
  , port         :: Natural
  }