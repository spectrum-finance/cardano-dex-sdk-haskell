module Cardano.Models where

import Ledger
import Ledger.Scripts      (Validator, MintingPolicy)
import Ledger.Tx
import Playground.Contract (FromJSON, ToJSON, Generic)

import Cardano.Types

-- TX template without collaterals, fees, change etc.
data TxOutCandidate = TxOutCandidate
  { txOutCandidateAddress         :: Address
  , txOutCandidateValue           :: Value
  , txOutCandidateDatum           :: Maybe Datum
  , txOutCandidateMintingPolicies :: [MintingPolicy]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FullTxOut = FullTxOut
  { fullTxOutGix     :: Gix
  , fullTxOutRef     :: TxOutRef
  , fullTxOutAddress :: Address
  , fullTxOutValue   :: Value
  , fullTxOutDatum   :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FullTxIn = FullTxIn
  { fullTxInTxOut    :: FullTxOut
  , fullTxInScript   :: Maybe Validator
  , fullTxInRedeemer :: Maybe Redeemer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TxCandidate = TxCandidate
  { txCandidateInputs  :: [FullTxIn]
  , txCandidateOutputs :: [TxOutCandidate]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
