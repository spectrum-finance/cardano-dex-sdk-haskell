module Explorer.Models where

import Data.Aeson     (FromJSON, ToJSON)
import GHC.Generics
import Ledger
import Explorer.Types

data Items a = Items
  { items :: [a]
  , total :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut
  { ref         :: OutRef
  , txHash      :: TxHash
  , index       :: Int
  , globalIndex :: Integer
  , addr        :: Addr
  , value       :: [OutAsset]
  , dataHash    :: Maybe Hash32
  , data'       :: Maybe Datum
  } deriving (Show, Generic, FromJSON, ToJSON)

data OutAsset = OutAsset
  { policy   :: PolicyId
  , name     :: AssetName
  , quantity :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON)
