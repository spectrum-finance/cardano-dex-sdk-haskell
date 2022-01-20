module NetworkAPI.Env where

import qualified Data.Set        as Set
import           Numeric.Natural
import           GHC.Natural

import          Cardano.Api
import          Cardano.Api.Shelley   (ProtocolParameters, PoolId)
import          Cardano.Slotting.Time (SystemStart)

import qualified Explorer.Models as Explorer

data SystemEnv = SystemEnv
  { pparams           :: ProtocolParameters
  , network           :: NetworkId
  , sysstart          :: SystemStart
  , pools             :: Set.Set PoolId
  , eraHistory        :: EraHistory CardanoMode
  , collateralPercent :: Natural
  }

toNetworkApiSystemEnv :: Explorer.SystemEnv -> SystemEnv
toNetworkApiSystemEnv input =
  SystemEnv 
    { pparams           = Explorer.pparams' input
    , network           = Explorer.network' input
    , sysstart          = Explorer.sysstart' input
    , pools             = Explorer.pools' input
    , eraHistory        = Explorer.eraHistory' input
    , collateralPercent = intToNatural $ Explorer.collateralPercent' input
    }