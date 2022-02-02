module Explorer.Service where

import Control.Monad.IO.Class
import Data.ByteString.Char8  as Data
import Data.Function
import Data.Aeson
import qualified  Data.Text as T
import GHC.Natural
import Network.HTTP.Simple

import Explorer.Types
import Explorer.Models
import Explorer.Config

data Explorer f = Explorer
  { getUnspentOutputs        :: Gix -> Limit -> f (Items FullTxOut)
  , getUnspentOutputsByPCred :: PaymentCred -> Paging -> f (Items FullTxOut)
  , getSystemEnv             :: f SystemEnv
  }

mkExplorer :: MonadIO f => ExplorerConfig -> Explorer f
mkExplorer conf = Explorer
  { getUnspentOutputs        = getUnspentOutputs' conf
  , getUnspentOutputsByPCred = getUnspentOutputsByPCred' conf
  , getSystemEnv             = getSystemEnv' conf
  }

getUnspentOutputs' :: MonadIO f => ExplorerConfig -> Gix -> Limit -> f (Items FullTxOut)
getUnspentOutputs' conf minIndex limit =
  mkGetRequest conf $ "/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit

getUnspentOutputsByPCred' :: MonadIO f => ExplorerConfig -> PaymentCred -> Paging -> f (Items FullTxOut)
getUnspentOutputsByPCred' conf pcred Paging{..} =
  mkGetRequest conf $ "/outputs/unspent/byPaymentCred/" ++ T.unpack (unPaymentCred pcred) ++  "/?offset=" ++ show offset ++ "&limit=" ++ show limit

getSystemEnv' :: MonadIO f => ExplorerConfig -> f SystemEnv
getSystemEnv' conf = mkGetRequest conf "/networkParams"

mkGetRequest :: (MonadIO f, FromJSON a, Show a) => ExplorerConfig -> String -> f a
mkGetRequest ExplorerConfig{..} path = do
  let
    request = defaultRequest
      & setRequestPath (Data.pack path)
      & setRequestHost (Data.pack explorerHost)
      & setRequestPort (naturalToInt explorerPort)

  response <- httpJSON request

  let parsedResponse = getResponseBody response

  liftIO . print $ "Response is: " ++ show parsedResponse

  pure parsedResponse
