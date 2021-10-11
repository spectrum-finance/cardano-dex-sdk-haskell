module Cardano.Utils where

import           Plutus.V1.Ledger.Tx
import           Cardano.Models

fromPkTxOut2TxIn :: ErgoTxOut -> TxIn
fromPkTxOut2TxIn ErgoTxOut{..} = TxIn {
    txInRef = txOutRef,
    txInType = Just ConsumePublicKeyAddress
}

fromScriptTxOut2TxIn :: ErgoTxOut -> TxIn
fromScriptTxOut2TxIn ErgoTxOut{..} = TxIn {
    txInRef = txOutRef,
    txInType = Nothing
}