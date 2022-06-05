module ErgoDex.Class where

import CardanoTx.Models
import ErgoDex.State

class FromLedger a where
  parseFromLedger :: FullTxOut -> Maybe (OnChain a)

class ToLedger a where
  toLedger :: a -> TxOutCandidate

class FromExplorer a b where
  parseFromExplorer :: a -> Maybe b