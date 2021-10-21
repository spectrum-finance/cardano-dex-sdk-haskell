module Explorer.Types where

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics

-- Bech32 encoded and rendered address
newtype Addr = Addr { unAddr :: String }
  deriving (Show, Generic, FromJSON, ToJSON)

-- Output reference string composed of TxHash and OutIndex : {TxHash}#{OutIndex}
newtype OutRef = OutRef { unOutRef :: String }
  deriving (Show, Generic, FromJSON, ToJSON)

-- A hex-encoded hash of minting policy
newtype PolicyId = PolicyId { unPolicyId :: String }
  deriving (Show, Generic, FromJSON, ToJSON)

-- Asset name represented as a utf-8 string
newtype AssetName = AssetName { unAssetName :: String }
  deriving (Show, Generic, FromJSON, ToJSON)

-- 32 bytes hash represented as a hex string
newtype Hash32 = Hash32 { unHash32 :: String }
  deriving (Show, Generic, FromJSON, ToJSON)

-- TX hash32 represented as a hex string
newtype TxHash = TxHash { unTxHash :: String }
  deriving (Show, Generic, FromJSON, ToJSON)
