module Cardano.Class where

import Cardano.Models

class FromLedger a where
  parseFromLedger :: FullTxOut -> Maybe a