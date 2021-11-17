module Explorer.Models where

import           Data.Aeson     (FromJSON)
import           GHC.Generics
import           Ledger
import           Explorer.Types
import           Explorer.Class
import qualified CardanoTx.Models as Tx

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
  , dataHash    :: Maybe Hash32
  , data'       :: Maybe Datum
  } deriving (Show, Generic, FromJSON)

instance ToCardanoTx Tx.FullTxOut FullTxOut where
  toCardanoTx FullTxOut{..} = undefined -- todo

data OutAsset = OutAsset
  { policy   :: PolicyId
  , name     :: AssetName
  , quantity :: Integer
  } deriving (Show, Generic, FromJSON)
