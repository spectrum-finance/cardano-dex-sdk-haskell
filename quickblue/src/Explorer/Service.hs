module Explorer.Service where

import Control.Monad.IO.Class
import Data.ByteString.Char8  as Data
import Data.Function
import GHC.Natural
import Network.HTTP.Simple

import Explorer.Types
import Explorer.Models
import Explorer.Config

data Explorer f = Explorer
  { getUspentOutputs :: Gix -> Limit -> f (Items FullTxOut)
  }

mkExplorer :: MonadIO f => ExplorerConfig -> Explorer f
mkExplorer conf = Explorer $ getUnspentOutputs' conf

getUnspentOutputs' :: MonadIO f => ExplorerConfig -> Gix -> Limit -> f (Items FullTxOut)
getUnspentOutputs' ExplorerConfig{..} minIndex limit = do
  let
    request = defaultRequest
      & setRequestPath (Data.pack $ "/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit)
      & setRequestHost (Data.pack explorerHost)
      & setRequestPort (naturalToInt explorerPort)

  response <- httpJSON request

  pure $ getResponseBody response
