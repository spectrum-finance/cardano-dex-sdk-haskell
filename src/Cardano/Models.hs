module Cardano.Models where

import Ledger

import Cardano.Types

data FullTxOut = FullTxOut
  { txOutGix     :: Gix
  , txOutRef     :: TxOutRef
  , txOutAddress :: Address
  , txOutValue   :: Value
  , txOutDatum   :: Maybe Datum
  } deriving (Show, Eq)
