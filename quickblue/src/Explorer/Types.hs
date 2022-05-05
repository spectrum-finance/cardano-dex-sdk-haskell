module Explorer.Types where

import Data.Aeson   (FromJSON, ToJSON)
import Data.Text    hiding (words, head)
import Data.String  (IsString(..))
import Data.Maybe   (fromMaybe)
import GHC.Generics

import qualified Ledger as P
import           Explorer.Class
import qualified CardanoTx.Address as Core

-- Bech32 encoded and rendered address
newtype Addr = Addr { unAddr :: Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

instance ToCardanoTx Addr P.Address where
  toCardanoTx (Addr addr) = fromMaybe (error "Impossible") (Core.readShellyAddress addr)

-- Payment credential encoded as a hex string
newtype PaymentCred = PaymentCred { unPaymentCred :: Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- Output reference string composed of TxHash and OutIndex : {TxHash}:{OutIndex}
newtype OutRef = OutRef { unOutRef :: Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

txOutRefSep :: Text
txOutRefSep = ":"

instance ToCardanoTx OutRef P.TxOutRef where
  toCardanoTx (OutRef ref) = P.TxOutRef (fromString $ unpack hash) (read $ unpack index)
    where [hash, index] = splitOn txOutRefSep ref

-- A hex-encoded hash of minting policy
newtype PolicyId = PolicyId { unPolicyId :: Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- Asset name represented as a utf-8 string
newtype AssetName = AssetName { unAssetName :: Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- 32 bytes hash represented as a hex string
newtype Hash32 = Hash32 { unHash32 :: Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- TX hash32 represented as a hex string
newtype TxHash = TxHash { unTxHash :: Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- A global index
newtype Gix = Gix { unGix :: Integer }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

newtype Limit = Limit { unLimit :: Integer }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON, ToJSON)
