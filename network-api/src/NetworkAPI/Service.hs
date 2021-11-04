module NetworkAPI.Service where

import Cardano.Ledger
import NetworkAPI.Env

data NetworkParams f = NetworkParams
  { getSystemEnv    :: f SystemEnv
  }
