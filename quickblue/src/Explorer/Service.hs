module Explorer.Service where

import Control.Monad.IO.Class
import Data.ByteString.Char8  as Data
import Data.Function
import Data.Aeson
import qualified  Data.Text as T
import Network.HTTP.Simple

import Explorer.Types
import Explorer.Models
import Explorer.Config

import Ledger ( TxOutRef, txOutRefId, txOutRefIdx )

data Explorer f = Explorer
  { getOutput                :: TxOutRef -> f (Maybe FullTxOut)
  , getUnspentOutputs        :: Gix -> Limit -> f (Items FullTxOut)
  , getUnspentOutputsByPCred :: PaymentCred -> Paging -> f (Items FullTxOut)
  , getSystemEnv             :: f SystemEnv
  }

mkExplorer :: MonadIO f => ExplorerConfig -> Explorer f
mkExplorer conf = Explorer
  { getOutput                = getOutput' conf
  , getUnspentOutputs        = getUnspentOutputs' conf
  , getUnspentOutputsByPCred = getUnspentOutputsByPCred' conf
  , getSystemEnv             = getSystemEnv' conf
  }

getOutput' :: MonadIO f => ExplorerConfig -> TxOutRef -> f (Maybe FullTxOut)
getOutput' conf ref = 
  mkGetRequest conf $ "/v1/outputs/" ++ renderTxOutRef ref

getUnspentOutputs' :: MonadIO f => ExplorerConfig -> Gix -> Limit -> f (Items FullTxOut)
getUnspentOutputs' conf minIndex limit =
  mkGetRequest conf $ "/v1/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit

getUnspentOutputsByPCred' :: MonadIO f => ExplorerConfig -> PaymentCred -> Paging -> f (Items FullTxOut)
getUnspentOutputsByPCred' conf pcred Paging{..} =
  mkGetRequest conf $ "/v1/outputs/unspent/byPaymentCred/" ++ T.unpack (unPaymentCred pcred) ++  "/?offset=" ++ show offset ++ "&limit=" ++ show limit

getSystemEnv' :: MonadIO f => ExplorerConfig -> f SystemEnv
getSystemEnv' conf = mkGetRequest conf "/networkParams"

mkGetRequest :: (MonadIO f, FromJSON a, Show a) => ExplorerConfig -> String -> f a
mkGetRequest ExplorerConfig{..} path = do
  let request = parseRequest_ (unUri explorerUri) & setRequestPath (Data.pack path)

  response <- httpJSON request

  let parsedResponse = getResponseBody response

  liftIO . print $ "Response is: " ++ show parsedResponse

  pure parsedResponse

renderTxOutRef ref = (show . txOutRefId $ ref) ++ "#" ++ (show . txOutRefIdx $ ref)
