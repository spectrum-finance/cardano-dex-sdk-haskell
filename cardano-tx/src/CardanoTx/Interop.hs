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
  , toCardanoTxOutV2
  , fromCardanoTxOutV2
  , toCardanoTxInWitnessV2
  ) where

import Data.Functor ((<&>))

import qualified Cardano.Api           as C
import qualified Cardano.Api.Shelley   as C
import qualified Ledger.Tx.CardanoAPI  as Interop
import qualified Ledger                as P
import qualified Plutus.V2.Ledger.Tx   as PV2
import           Ledger.Tx.CardanoAPI  (toCardanoAddressInEra, FromCardanoError)
import           Ledger.Ada            (lovelaceValueOf)

import Plutus.Contract.CardanoAPI 
  ( toCardanoTxOutValue, ToCardanoError (SimpleScriptsNotSupportedToCardano), toCardanoPlutusScript, toCardanoScriptData, zeroExecutionUnits )

import CardanoTx.Models  (FullTxOut(..), TxOutCandidate(..), TxOutDatum(..))

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

extractCardanoTxOutputs :: C.Tx era -> [FullTxOut]
extractCardanoTxOutputs tx =
  let
    collectOutputs acc ix =
      case extractCardanoTxOutputAt ix tx of
        Just out -> collectOutputs (out : acc) (ix + 1)
        Nothing  -> reverse acc
  in collectOutputs [] 0

extractCardanoTxBodyOutputs :: C.TxBody era -> [(Int, TxOutCandidate)]
extractCardanoTxBodyOutputs txb =
  case txb of
    C.TxBody bodyc -> extractCardanoTxContentOutputs bodyc

extractCardanoTxContentOutputs :: C.TxBodyContent mode era -> [(Int, TxOutCandidate)]
extractCardanoTxContentOutputs bodyc =
    zip [0..] $ C.txOuts bodyc >>= (maybe [] pure . toSdkOutput)
  where
    toSdkOutput o = do
      PV2.TxOut{..} <- toCardanoTxOutput o
      pure $ TxOutCandidate txOutAddress txOutValue (fromPlutusDatum txOutDatum)

fromPlutusDatum :: PV2.OutputDatum -> TxOutDatum
fromPlutusDatum PV2.NoOutputDatum = EmptyDatum
fromPlutusDatum (PV2.OutputDatumHash dh) = KnownDatumHash dh
fromPlutusDatum (PV2.OutputDatum d) = KnownDatum d

extractCardanoTxOutputAt :: Int -> C.Tx era -> Maybe FullTxOut
extractCardanoTxOutputAt ix tx = do
  PV2.TxOut{..} <- getOutputAt ix tx >>= toCardanoTxOutput
  let
    txId = extractCardanoTxId tx
    ref  = P.TxOutRef txId (toInteger ix)
  pure $ FullTxOut ref txOutAddress txOutValue (fromPlutusDatum txOutDatum)

toCardanoTxInWitnessV2 :: P.TxInType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitnessV2 P.ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitnessV2 P.ConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitnessV2
    (P.ConsumeScriptAddress
        P.PlutusV1
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) validator)
        <*> pure (C.ScriptDatumForTxIn $ toCardanoScriptData datum)
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )
toCardanoTxInWitnessV2
    (P.ConsumeScriptAddress
        P.PlutusV2
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))  -- todo: add match
    = C.ScriptWitness C.ScriptWitnessForSpending <$>
        (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
        <$> fmap C.PScript (toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) validator)
        <*> pure C.InlineScriptDatum
        <*> pure (toCardanoScriptData redeemer)
        <*> pure zeroExecutionUnits
        )

getOutputAt :: Int -> C.Tx era -> Maybe (C.TxOut C.CtxTx era)
getOutputAt ix tx =
  case C.getTxBody tx of
    C.TxBody bodyc ->
      let outs = C.txOuts bodyc
      in if length outs < ix + 1 then Nothing else Just $ outs !! ix

toCardanoTxOutput :: C.TxOut C.CtxTx era -> Maybe PV2.TxOut
toCardanoTxOutput o = either (const Nothing) Just (fromCardanoTxOutV2 o)

toCardanoTxOutV2
    :: C.NetworkId
    -> (PV2.OutputDatum -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> PV2.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOutV2 networkId fromDatum (PV2.TxOut addr value datum _) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValue value
            <*> fromDatum datum
            <*> pure C.ReferenceScriptNone

fromCardanoTxOutV2 :: C.TxOut C.CtxTx era -> Either FromCardanoError PV2.TxOut
fromCardanoTxOutV2 (C.TxOut addr value datum _) =
    PV2.TxOut
    <$> Interop.fromCardanoAddressInEra addr
    <*> pure (fromCardanoTxOutValue value)
    <*> pure (Interop.fromCardanoTxOutDatum datum)
    <*> pure Nothing

fromCardanoTxOutValue :: C.TxOutValue era -> P.Value
fromCardanoTxOutValue (C.TxOutAdaOnly _ lovelace) = fromCardanoLovelace lovelace
fromCardanoTxOutValue (C.TxOutValue _ value)      = Interop.fromCardanoValue value

fromCardanoLovelace :: C.Lovelace -> P.Value
fromCardanoLovelace (C.lovelaceToQuantity -> C.Quantity lovelace) = lovelaceValueOf lovelace
