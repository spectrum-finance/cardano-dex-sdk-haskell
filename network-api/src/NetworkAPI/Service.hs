module NetworkAPI.Service where

import qualified Cardano.Api    as C
import           NetworkAPI.Env
import           NetworkAPI.Config.NodeConfig
import           Cardano.Api
import           RIO
import           Network.HTTP.Simple
import           Data.ByteString.Char8
import           GHC.Natural
import qualified Data.ByteString.Lazy as Lazy

data Network f = Network
  { getSystemEnv :: f SystemEnv
  , submitTx     :: C.Tx C.AlonzoEra -> f ()
  }

mkNetwork :: (MonadIO f) => NodeConfig -> Network f
mkNetwork cfg = Network undefined (submitTx' cfg)

submitTx' :: (MonadIO f) => NodeConfig -> C.Tx C.AlonzoEra -> f ()
submitTx' NodeConfig{..} tx = do
  let
    serialisedTx = Lazy.fromStrict $ serialiseToCBOR tx

    request = defaultRequest
      & setRequestPath (pack "api/submit/tx")
      & setRequestHost (pack host)
      & setRequestPort (naturalToInt port)
      & setRequestMethod (pack "POST")
      & setRequestBodyLBS serialisedTx

  void $ liftIO $ httpNoBody request