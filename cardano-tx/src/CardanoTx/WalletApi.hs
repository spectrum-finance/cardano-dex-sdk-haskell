module CardanoTx.WalletApi where

import Plutus.V1.Ledger.Tx as P
import Ledger

data Constr
  = ValueAtLeast Value
  | ValueAtMost Value

data WalletApi f = WalletApi
  { selectUtxos  :: Maybe Constr -> f [P.TxOut]
  , addSignature :: P.Tx -> f P.Tx
  }
