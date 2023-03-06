module ErgoDex.Class where

import CardanoTx.Models
import ErgoDex.State

class FromLedger a where
  parseFromLedger :: FullTxOut -> Maybe (OnChain a)

class ToLedger ctx a where
  toLedger :: ctx -> a -> TxOutCandidate