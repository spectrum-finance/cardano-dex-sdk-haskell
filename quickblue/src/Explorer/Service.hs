module Explorer.Service where

import Control.Monad.IO.Class
import Data.ByteString.Char8  as Data
import Data.Function
import Data.Aeson
import GHC.Natural
import Network.HTTP.Simple

import Explorer.Types
import Explorer.Models
import Explorer.Config

data Explorer f = Explorer
  { getUnspentOutputs          :: Gix -> Limit -> f (Items FullTxOut)
  , getUnspentOutputsByAddress :: Addr -> Paging -> f (Items FullTxOut)
  }

mkExplorer :: MonadIO f => ExplorerConfig -> Explorer f
mkExplorer conf = Explorer
  { getUnspentOutputs          = getUnspentOutputs' conf
  , getUnspentOutputsByAddress = getUnspentOutputsByAddress' conf
  }

getUnspentOutputs' :: MonadIO f => ExplorerConfig -> Gix -> Limit -> f (Items FullTxOut)
getUnspentOutputs' conf minIndex limit =
  mkGetRequest conf $ "/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit

getUnspentOutputsByAddress' :: MonadIO f => ExplorerConfig -> Addr -> Paging -> f (Items FullTxOut)
getUnspentOutputsByAddress' conf addr Paging{..} =
  mkGetRequest conf $ "/outputs/unspent/addr/" ++ show addr ++  "/?offset=" ++ show offset ++ "&limit=" ++ show limit

mkGetRequest :: (MonadIO f, FromJSON a) => ExplorerConfig -> String -> f a
mkGetRequest ExplorerConfig{..} path = do
  let
    request = defaultRequest
      & setRequestPath (Data.pack path)
      & setRequestHost (Data.pack explorerHost)
      & setRequestPort (naturalToInt explorerPort)

  response <- httpJSON request

  pure $ getResponseBody response
