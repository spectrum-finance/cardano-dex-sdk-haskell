module Spectrum.LedgerSync.Protocol.Data.MempoolSync where

import GHC.Generics

import Ouroboros.Consensus.Cardano.Block 
  ( GenTx )
import Cardano.Api 
  ( SlotNo )

data MempoolRequest block =
  RequestNextTx
  deriving (Generic, Show, Eq)

data MempoolResponse block =
  NewTx (GenTx block) SlotNo
  deriving (Generic)