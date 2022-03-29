module CardanoTx.Interop
  ( extractCardanoTxId
  , extractCardanoInputs
  , extractCardanoTxBodyInputs
  , extractCardanoTxBodyOutputs
  , extractCardanoTxOutputAt
  , getOutputAt
  ) where

import Data.Functor ((<&>))

import qualified Cardano.Api          as C
import qualified Ledger.Tx.CardanoAPI as Interop
import qualified Ledger               as P

import CardanoTx.Models (FullTxOut(..), TxOutCandidate(..), OutDatum(..))

extractCardanoTxId :: C.Tx era -> P.TxId
extractCardanoTxId = Interop.fromCardanoTxId . C.getTxId . C.getTxBody

extractCardanoInputs :: C.Tx era -> [P.TxOutRef]
extractCardanoInputs = extractCardanoTxBodyInputs . C.getTxBody

extractCardanoTxBodyInputs :: C.TxBody era -> [P.TxOutRef]
extractCardanoTxBodyInputs txb =
  case txb of
    C.TxBody bodyc -> C.txIns bodyc <&> (Interop.fromCardanoTxIn . fst)

extractCardanoTxBodyOutputs :: C.TxBody era -> [(Int, TxOutCandidate)]
extractCardanoTxBodyOutputs txb =
  case txb of
    C.TxBody bodyc -> zip [0..] $ C.txOuts bodyc >>= (maybe [] pure . toSdkOutput)
  where
    toSdkOutput o = do
      P.TxOut{..} <- toCardanoTxOutput o
      let dt = maybe UnitDatum KnownDatumHash txOutDatumHash
      pure $ TxOutCandidate txOutAddress txOutValue dt

extractCardanoTxOutputAt :: Int -> C.Tx era -> Maybe FullTxOut
extractCardanoTxOutputAt ix tx = do
  P.TxOut{..} <- getOutputAt ix tx >>= toCardanoTxOutput
  let
    txId = extractCardanoTxId tx
    ref  = P.TxOutRef txId (toInteger ix)
    dt   = maybe UnitDatum KnownDatumHash txOutDatumHash
  pure $ FullTxOut ref txOutAddress txOutValue dt

getOutputAt :: Int -> C.Tx era -> Maybe (C.TxOut C.CtxTx era)
getOutputAt ix tx =
  case C.getTxBody tx of
    C.TxBody bodyc ->
      let outs = C.txOuts bodyc
      in if length outs < ix + 1 then Nothing else Just $ outs !! ix

toCardanoTxOutput :: C.TxOut C.CtxTx era -> Maybe P.TxOut
toCardanoTxOutput o = either (const Nothing) Just (Interop.fromCardanoTxOut o)
