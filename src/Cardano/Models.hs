{-# LANGUAGE DuplicateRecordFields   #-}

module Cardano.Models where

import Ledger
import Plutus.V1.Ledger.Tx

import Cardano.Types

data ErgoTxOut = ErgoTxOut
  { txOutGix     :: Gix
  , txOutRef     :: TxOutRef
  , txOutAddress :: Address
  , txOutValue   :: Value
  , txOutDatum   :: Maybe Datum
  } deriving (Show, Eq)

data TxOutCandidate = TxOutCandidate
	{ address :: Address
  , value   :: Value
  , datum   :: Maybe Datum
	}

data TxCandidate = TxCandidate
	{ inputs    :: [TxIn]
	, outputs   :: [TxOutCandidate]
	}