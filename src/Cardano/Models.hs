module Cardano.Models where

import Ledger
import Ledger.Tx

import Cardano.Types

data FullTxOut = FullTxOut
  { txOutGix     :: Gix
  , txOutRef     :: TxOutRef
  , txOutAddress :: Address
  , txOutValue   :: Value
  , txOutDatum   :: Maybe Datum
  } deriving (Show, Eq)

data FullTxIn = FullTxIn
  { txInTxOut    :: FullTxOut
  , txInRedeemer :: Maybe Redeemer
  } deriving (Show, Eq)

data TxCandidate = TxCandidate
  { txCandidateInputs  :: [FullTxIn]
  , txCandidateOutputs :: [FullTxOut]
  } deriving (Show, Eq)
