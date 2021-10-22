module Cardano.Models where

import Ledger
import Playground.Contract (FromJSON, ToJSON, Generic)

-- Defines how a residual value (if any) should be handled
data ChangePolicy =
    ReturnTo Address
  | ReturnAtLeastTo [(Address, Value)] -- Specifies a list of (change receiver addr, max value to return)
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- TX output template
data TxOutCandidate = TxOutCandidate
  { txOutCandidateAddress  :: Address
  , txOutCandidateValue    :: Value
  , txOutCandidateDatum    :: Maybe Datum
  , txOutCandidatePolicies :: [MintingPolicy]
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
  , txCandidateChangePolicy :: Maybe ChangePolicy
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
