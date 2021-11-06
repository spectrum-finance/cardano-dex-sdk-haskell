module NetworkAPI.Service where

import qualified Cardano.Api    as C
import           NetworkAPI.Env

data Network f = Network
  { getSystemEnv :: f SystemEnv
  , submitTx     :: C.Tx C.AlonzoEra -> f ()
  }
