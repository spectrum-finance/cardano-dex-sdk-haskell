module CardanoTx.Types where

import qualified Data.Text  as T
import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)

-- TX hash32 represented as a hex string
newtype TxHash = TxHash { unTxHash :: T.Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)

-- Block hash32 represented as a hex string
newtype BlockHash = BlockHash { unBlockHash :: T.Text }
  deriving (Eq, Generic)
  deriving newtype (Show, FromJSON)