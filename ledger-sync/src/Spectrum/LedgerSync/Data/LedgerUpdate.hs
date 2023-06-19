module Spectrum.LedgerSync.Data.LedgerUpdate where

import Ouroboros.Network.Block
  ( Point (..), Tip (..) )

data LedgerUpdate block
  = RollForward block (Tip block)
  | RollBackward (Point block)
  deriving (Eq, Show)
