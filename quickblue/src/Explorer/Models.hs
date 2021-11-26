module Explorer.Models where

import           Data.Aeson   (FromJSON)
import           Data.String  (IsString(..))
import qualified Data.Text    as T
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
  } deriving (Show, Generic, FromJSON)

instance ToCardanoTx FullTxOut Tx.FullTxOut where
  toCardanoTx FullTxOut{..} = Tx.FullTxOut
    { fullTxOutRef       = toCardanoTx ref
    , fullTxOutAddress   = toCardanoTx addr
    , fullTxOutValue     = foldr (\a -> \acc -> acc <> toCardanoTx a) mempty value
    , fullTxOutDatumHash = dataHash
    , fullTxOutDatum     = data'
    }

data OutAsset = OutAsset
  { policy   :: PolicyId
  , name     :: AssetName
  , quantity :: Integer
  } deriving (Show, Generic, FromJSON)

instance ToCardanoTx OutAsset P.Value where
  toCardanoTx OutAsset{..} = Value.singleton p n quantity
    where
      p = fromString $ T.unpack $ unPolicyId policy
      n = fromString $ T.unpack $ unAssetName name
