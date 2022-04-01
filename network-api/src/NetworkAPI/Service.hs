module NetworkAPI.Service
  ( Network(..)
  ) where

import Cardano.Api
import NetworkAPI.Types (SystemEnv)

data Network f era = Network
  { getSystemEnv :: f SystemEnv
  , submitTx     :: Tx era -> f ()
  }
