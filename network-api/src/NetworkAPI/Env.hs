module NetworkAPI.Env where

import Data.Set (Set)

import Cardano.Api           (CardanoMode, NetworkId, EraHistory)
import Cardano.Api.Shelley   (ProtocolParameters, PoolId)
import Cardano.Slotting.Time (SystemStart)

import qualified Explorer.Models as Explorer

data SystemEnv = SystemEnv
  { pparams    :: ProtocolParameters
  , network    :: NetworkId
  , sysstart   :: SystemStart
  , pools      :: Set PoolId
  , eraHistory :: EraHistory CardanoMode
  }

toNetworkApiSystemEnv :: Explorer.SystemEnv -> SystemEnv
toNetworkApiSystemEnv explorerSysenv =
  SystemEnv 
    { pparams    = Explorer.pparams' explorerSysenv
    , network    = Explorer.network' explorerSysenv
    , sysstart   = Explorer.sysstart' explorerSysenv
    , pools      = Explorer.pools' explorerSysenv
    , eraHistory = Explorer.eraHistory' explorerSysenv
    }
