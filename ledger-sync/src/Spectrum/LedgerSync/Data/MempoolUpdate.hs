module Spectrum.LedgerSync.Data.MempoolUpdate where
    
import Ouroboros.Consensus.Cardano.Block 
  ( GenTx )
import Cardano.Api
  ( SlotNo )

data MempoolUpdate block =
    NewTx (GenTx block) SlotNo