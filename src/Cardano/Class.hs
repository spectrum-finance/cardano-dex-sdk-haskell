module Cardano.Class where

import Data.Kind
import Cardano.Models

class FromLedger (a :: Type) where
  parseFromLedger :: FullTxOut -> Maybe a