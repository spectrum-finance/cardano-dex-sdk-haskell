module NetworkAPI.Env where

import qualified Data.Set        as Set
import           Numeric.Natural

import Cardano.Api
import Cardano.Api.Shelley   (ProtocolParameters, PoolId)
import Cardano.Slotting.Time (SystemStart)

data SystemEnv = SystemEnv
  { pparams           :: ProtocolParameters
  , network           :: NetworkId
  , sysstart          :: SystemStart
  , pools             :: Set.Set PoolId
  , eraHistory        :: EraHistory CardanoMode
  , collateralPercent :: Natural
  }
