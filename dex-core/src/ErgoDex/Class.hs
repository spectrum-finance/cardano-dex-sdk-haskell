module ErgoDex.Class where

import CardanoTx.Models
import ErgoDex.State

class FromLedger a where
  parseFromLedger :: FullTxOut -> Maybe (Confirmed a)

class ToLedger a where
  toLedger :: a -> TxOutCandidate
