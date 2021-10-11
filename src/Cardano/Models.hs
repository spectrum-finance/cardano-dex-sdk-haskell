module Cardano.Models where

import Ledger

import Cardano.Types

data TxOut = TxOut
  { txOutGix     :: Gix
  , txOutRef     :: TxOutRef
  , txOutAddress :: Address
  , txOutValue   :: Value
  , txOutDatum   :: Maybe Datum
  } deriving (Show, Eq)
