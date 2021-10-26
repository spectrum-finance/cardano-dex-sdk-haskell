module CardanoTx.Models where

import Ledger
import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)

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

data FullTxIn = FullTxIn
  { fullTxInTxOut    :: FullTxOut
  , fullTxInScript   :: Maybe Validator
  , fullTxInRedeemer :: Maybe Redeemer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- TX template without collaterals, fees, change etc.
data TxCandidate = TxCandidate
  { txCandidateInputs       :: [FullTxIn]
  , txCandidateOutputs      :: [TxOutCandidate]
  , txCandidateValueMint    :: MintValue
  , txCandidatePolicies     :: [MintingPolicy]
  , txCandidateChangePolicy :: Maybe ChangePolicy
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
