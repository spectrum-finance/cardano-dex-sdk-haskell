module CardanoTx.Interop
  ( extractCardanoTxId
  , extractCardanoInputs
  , extractCardanoTxBodyInputs
  , extractCardanoTxContentInputs
  , extractCardanoTxOutputs
  , extractCardanoTxBodyOutputs
  , extractCardanoTxContentOutputs
  , extractCardanoTxOutputAt
  , getOutputAt
  ) where

import Data.Functor ((<&>))

import qualified Cardano.Api          as C
import qualified Ledger.Tx.CardanoAPI as Interop
import qualified Ledger               as P

import CardanoTx.Models (FullTxOut(..), TxOutCandidate(..), TxOutDatum(..))

extractCardanoTxId :: C.Tx era -> P.TxId
extractCardanoTxId = Interop.fromCardanoTxId . C.getTxId . C.getTxBody

extractCardanoInputs :: C.Tx era -> [P.TxOutRef]
extractCardanoInputs = extractCardanoTxBodyInputs . C.getTxBody

extractCardanoTxBodyInputs :: C.TxBody era -> [P.TxOutRef]
extractCardanoTxBodyInputs txb =
  case txb of
    C.TxBody bodyc -> extractCardanoTxContentInputs bodyc

extractCardanoTxContentInputs :: C.TxBodyContent mode era -> [P.TxOutRef]
extractCardanoTxContentInputs bodyc = C.txIns bodyc <&> (Interop.fromCardanoTxIn . fst)

extractCardanoTxOutputs :: C.Tx era -> [(Int, TxOutCandidate)]
extractCardanoTxOutputs = extractCardanoTxBodyOutputs . C.getTxBody

extractCardanoTxBodyOutputs :: C.TxBody era -> [(Int, TxOutCandidate)]
extractCardanoTxBodyOutputs txb =
  case txb of
    C.TxBody bodyc -> extractCardanoTxContentOutputs bodyc

extractCardanoTxContentOutputs :: C.TxBodyContent mode era -> [(Int, TxOutCandidate)]
extractCardanoTxContentOutputs bodyc =
    zip [0..] $ C.txOuts bodyc >>= (maybe [] pure . toSdkOutput)
  where
    toSdkOutput o = do
      P.TxOut{..} <- toCardanoTxOutput o
      let dt = maybe EmptyDatum KnownDatumHash txOutDatumHash
      pure $ TxOutCandidate txOutAddress txOutValue dt

extractCardanoTxOutputAt :: Int -> C.Tx era -> Maybe FullTxOut
extractCardanoTxOutputAt ix tx = do
  P.TxOut{..} <- getOutputAt ix tx >>= toCardanoTxOutput
  let
    txId = extractCardanoTxId tx
    ref  = P.TxOutRef txId (toInteger ix)
    dt   = maybe EmptyDatum KnownDatumHash txOutDatumHash
  pure $ FullTxOut ref txOutAddress txOutValue dt

getOutputAt :: Int -> C.Tx era -> Maybe (C.TxOut C.CtxTx era)
getOutputAt ix tx =
  case C.getTxBody tx of
    C.TxBody bodyc ->
      let outs = C.txOuts bodyc
      in if length outs < ix + 1 then Nothing else Just $ outs !! ix

toCardanoTxOutput :: C.TxOut C.CtxTx era -> Maybe P.TxOut
toCardanoTxOutput o = either (const Nothing) Just (Interop.fromCardanoTxOut o)
