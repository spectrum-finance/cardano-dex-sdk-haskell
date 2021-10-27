module CardanoTx.Models where

import           Ledger
import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

-- Defines how a residual value (if any) should be handled
data ChangePolicy = ReturnTo Address
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

newtype MintValue = MintValue { unMintValue :: Value }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- TX output template
data TxOutCandidate = TxOutCandidate
  { txOutCandidateAddress  :: Address
  , txOutCandidateValue    :: Value
  , txOutCandidateDatum    :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut
  { fullTxOutRef       :: TxOutRef
  , fullTxOutAddress   :: Address
  , fullTxOutValue     :: Value
  , fullTxOutDatumHash :: Maybe DatumHash
  , fullTxOutDatum     :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance Ord FullTxOut where
  compare FullTxOut{fullTxOutRef=rx} FullTxOut{fullTxOutRef=ry} = compare rx ry

data FullTxIn = FullTxIn
  { fullTxInTxOut    :: FullTxOut
  , fullTxInScript   :: Maybe Validator
  , fullTxInRedeemer :: Maybe Redeemer
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- TX template without collaterals, fees, change etc.
data TxCandidate = TxCandidate
  { txCandidateInputs       :: Set.Set FullTxIn
  , txCandidateOutputs      :: [TxOutCandidate]
  , txCandidateValueMint    :: MintValue
  , txCandidateMintPolicies :: Set.Set MintingPolicy
  , txCandidateChangePolicy :: Maybe ChangePolicy
  , txCandidateValidRange   :: SlotRange
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
