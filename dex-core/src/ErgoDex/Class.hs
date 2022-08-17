module ErgoDex.Class where

import CardanoTx.Models
import ErgoDex.State

class FromLedger a where
  parseFromLedger :: FullTxOut -> Maybe (OnChain a)

class ToLedger m a where
  toLedger :: a -> m TxOutCandidate