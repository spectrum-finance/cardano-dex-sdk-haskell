module Explorer.Types where

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics

-- Bech32 encoded and rendered address
newtype Addr = Addr { unAddr :: String }
  deriving (Show, Generic, FromJSON)

-- Output reference string composed of TxHash and OutIndex : {TxHash}#{OutIndex}
newtype OutRef = OutRef { unOutRef :: String }
  deriving (Show, Generic, FromJSON)

-- A hex-encoded hash of minting policy
newtype PolicyId = PolicyId { unPolicyId :: String }
  deriving (Show, Generic, FromJSON)

-- Asset name represented as a utf-8 string
newtype AssetName = AssetName { unAssetName :: String }
  deriving (Show, Generic, FromJSON)

-- 32 bytes hash represented as a hex string
newtype Hash32 = Hash32 { unHash32 :: String }
  deriving (Show, Generic, FromJSON)

-- TX hash32 represented as a hex string
newtype TxHash = TxHash { unTxHash :: String }
  deriving (Show, Generic, FromJSON)

-- A global index
newtype Gix = Gix { unGix :: Integer }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype Limit = Limit { unLimit :: Integer }
  deriving (Show, Generic, FromJSON)
