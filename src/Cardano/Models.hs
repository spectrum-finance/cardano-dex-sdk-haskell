module Cardano.Models where

import Ledger
import Ledger.Scripts      (Validator, MintingPolicy)
import Ledger.Tx
import Playground.Contract (FromJSON, ToJSON, Generic)

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
  { txCandidateInputs  :: [FullTxIn]
  , txCandidateOutputs :: [TxOutCandidate]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
