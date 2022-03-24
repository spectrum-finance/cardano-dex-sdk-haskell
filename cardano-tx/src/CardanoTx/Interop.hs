module CardanoTx.Interop
  ( extractCardanoTxId
  , extractCardanoTxOutputAt
  , getOutputAt
  ) where

import qualified Cardano.Api          as C
import qualified Ledger.Tx.CardanoAPI as Interop
import qualified Ledger               as P

import CardanoTx.Models (FullTxOut(FullTxOut))

extractCardanoTxId :: C.Tx era -> P.TxId
extractCardanoTxId = Interop.fromCardanoTxId . C.getTxId . C.getTxBody

extractCardanoTxOutputAt :: Int -> C.Tx era -> Maybe FullTxOut
extractCardanoTxOutputAt ix tx = do
    P.TxOut{..} <- getOutputAt ix tx >>= toCardanoTxOutput
    let
      txId = extractCardanoTxId tx
      ref  = P.TxOutRef txId (toInteger ix)
    pure $ FullTxOut ref txOutAddress txOutValue txOutDatumHash Nothing

getOutputAt :: Int -> C.Tx era -> Maybe (C.TxOut C.CtxTx era)
getOutputAt ix tx =
  case C.getTxBody tx of
    C.TxBody bodyc ->
      let outs = C.txOuts bodyc
      in if length outs < ix + 1 then Nothing else Just $ outs !! ix

toCardanoTxOutput :: C.TxOut C.CtxTx era -> Maybe P.TxOut
toCardanoTxOutput o = either (const Nothing) Just (Interop.fromCardanoTxOut o)
