module Cardano.Models where

import Ledger
import Ledger.Tx
import Playground.Contract (FromJSON, ToJSON, Generic)

import Cardano.Types

data FullTxOut = FullTxOut
  { fullTxOutGix     :: Gix
  , fullTxOutRef     :: TxOutRef
  , fullTxOutAddress :: Address
  , fullTxOutValue   :: Value
  , fullTxOutDatum   :: Maybe Datum
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TxInTarget = Pay2Script | Pay2PubKey
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data FullTxIn = FullTxIn
  { fullTxInTxOut    :: FullTxOut
  , fullTxInTarget   :: TxInTarget
  , fullTxInRedeemer :: Maybe Redeemer
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data TxCandidate = TxCandidate
  { txCandidateInputs  :: [FullTxIn]
  , txCandidateOutputs :: [FullTxOut]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
