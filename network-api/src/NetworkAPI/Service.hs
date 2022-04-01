module NetworkAPI.Service where

import qualified Cardano.Api    as C
import           NetworkAPI.Env ( SystemEnv )

data Network f era = Network
  { getSystemEnv :: f SystemEnv
  , submitTx     :: C.Tx era -> f ()
  }
