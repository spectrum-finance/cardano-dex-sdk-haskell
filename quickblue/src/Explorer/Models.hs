{-# LANGUAGE RecordWildCards #-}

module Explorer.Models where

import           Data.Aeson        (FromJSON)
import           Data.Aeson.Types
import           Data.String       (IsString(..))
import qualified Data.Text         as T
import           GHC.Generics

import qualified Ledger                 as P
import qualified Plutus.V1.Ledger.Value as Value
import           Explorer.Types
import           Explorer.Class
import qualified CardanoTx.Models       as Tx

data Paging = Paging
  { offset :: Int
  , limit  :: Int
  } deriving (Show, Generic, FromJSON)

data Items a = Items
  { items :: [a]
  , total :: Int
  } deriving (Show, Generic, FromJSON)

data FullTxOut = FullTxOut
  { ref         :: OutRef
  , txHash      :: TxHash
  , index       :: Int
  , globalIndex :: Gix
  , addr        :: Addr
  , value       :: [OutAsset]
  , dataHash    :: Maybe P.DatumHash
  , data'       :: Maybe P.Datum
  } deriving (Show, Generic)

instance FromJSON FullTxOut where
  parseJSON = withObject "quickblueFullTxOut" $ \o -> do
    ref          <- OutRef <$> o .: "ref"
    txHash       <- TxHash <$> o .: "txHash"
    index        <- o .: "index"
    globalIndex  <- Gix <$> o .: "globalIndex"
    addr         <- Addr <$> o .: "addr"
    value        <- o .: "value"
    dataHash     <- o .:? "dataHash"
    data'        <- o .:? "data"
    return FullTxOut{..}


instance ToCardanoTx FullTxOut Tx.FullTxOut where
  toCardanoTx FullTxOut{..} = Tx.FullTxOut
    { fullTxOutRef       = toCardanoTx ref
    , fullTxOutAddress   = toCardanoTx addr
    , fullTxOutValue     = foldr (\a -> \acc -> acc <> toCardanoTx a) mempty value
    , fullTxOutDatumHash = dataHash
    , fullTxOutDatum     = data'
    }

data OutAsset = OutAsset
  { policy      :: PolicyId
  , name        :: AssetName
  , quantity    :: Integer
  } deriving (Show, Generic)

instance FromJSON OutAsset where
  parseJSON = withObject "quickblueOutAsset" $ \o -> do
    policy    <- PolicyId <$> o .: "policyId"
    name      <- AssetName <$> o .: "name"
    quantity  <- o .: "quantity"
    return OutAsset{..}

instance ToCardanoTx OutAsset P.Value where
  toCardanoTx OutAsset{..} = Value.singleton p n quantity
    where
      p = fromString $ T.unpack $ unPolicyId policy
      n = fromString $ T.unpack $ unAssetName name
