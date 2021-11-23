module DatumKeeper.Client where

import RIO
import Network.HTTP.Simple
import Data.Aeson
import DatumKeeper.Config.KeeperConfig
import Data.ByteString.Char8
import GHC.Natural
import Ledger

data KeeperClient f = KeeperClient
  { putDatum :: Datum -> f ()
  }

makeKeeperClient :: (MonadIO f) => DatumKeeperConfig -> KeeperClient f
makeKeeperClient config = KeeperClient (putDatum' config)

putDatum' :: (MonadIO f) => DatumKeeperConfig -> Datum -> f ()
putDatum' DatumKeeperConfig{..} datum = do
  let
    request = defaultRequest
      & setRequestPath (pack "put")
      & setRequestHost (pack host)
      & setRequestPort (naturalToInt port)
      & setRequestMethod (pack "POST")
      & setRequestBodyJSON (toJSON datum)

  response <- liftIO $ httpJSON request

  pure $ getResponseBody response