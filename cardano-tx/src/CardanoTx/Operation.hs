module CardanoTx.Operation where

import Ouroboros.Consensus.Cardano.Block
import Cardano.Ledger.Crypto
    ( StandardCrypto(..) )
-- executed order 
-- created order 
-- new pool state 

parseUserOp :: CardanoBlock StandardCrypto -> Maybe ()

parseUserOp (BlockAlonzo m) = Just ()
parseUserOp _ = Nothing
