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
  , fromCardanoScriptHash
  , refScriptToScriptHash
  ) where

import Data.Functor       ((<&>))
import qualified Data.Map as Map

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
import qualified PlutusTx.Builtins as PlutusTx
import Data.Map (Map)
import Plutus.Script.Utils.V2.Scripts (scriptHash)

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
      pure $ TxOutCandidate txOutAddress txOutValue (fromPlutusDatum txOutDatum) txOutReferenceScript

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
  pure $ FullTxOut ref txOutAddress txOutValue (fromPlutusDatum txOutDatum) txOutReferenceScript

toCardanoTxInWitnessV2 :: Map P.Script C.TxIn -> P.TxInType -> Either ToCardanoError (C.Witness C.WitCtxTxIn C.BabbageEra)
toCardanoTxInWitnessV2 _ P.ConsumePublicKeyAddress = pure (C.KeyWitness C.KeyWitnessForSpending)
toCardanoTxInWitnessV2 _ P.ConsumeSimpleScriptAddress = Left SimpleScriptsNotSupportedToCardano -- TODO: Better support for simple scripts
toCardanoTxInWitnessV2
    _
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
    refMap
    (P.ConsumeScriptAddress
        P.PlutusV2
        (P.Validator validator)
        (P.Redeemer redeemer)
        (P.Datum datum))  -- todo: add match
    = do
      let scripts = Map.mapKeys P.scriptHash refMap
      case (Map.lookup (P.scriptHash validator) scripts) of
        Just txIn ->
          C.ScriptWitness C.ScriptWitnessForSpending <$>
            (pure (C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2 (C.PReferenceScript txIn Nothing))
            <*> pure C.InlineScriptDatum
            <*> pure (toCardanoScriptData redeemer)
            <*> pure zeroExecutionUnits
            )
        Nothing ->
          C.ScriptWitness C.ScriptWitnessForSpending <$>
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
    -> Map P.Script C.TxIn
    -> (PV2.OutputDatum -> Either ToCardanoError (C.TxOutDatum ctx C.BabbageEra))
    -> PV2.TxOut
    -> Either ToCardanoError (C.TxOut ctx C.BabbageEra)
toCardanoTxOutV2 networkId scriptsMap fromDatum (PV2.TxOut addr value datum refScript) =
    C.TxOut <$> toCardanoAddressInEra networkId addr
            <*> toCardanoTxOutValue value
            <*> fromDatum datum
            <*> fromRefScript refScript scriptsMap

fromRefScript :: Maybe P.ScriptHash -> Map P.Script C.TxIn -> Either ToCardanoError (C.ReferenceScript C.BabbageEra)
fromRefScript Nothing   _ = pure C.ReferenceScriptNone
fromRefScript (Just sh) scriptsMap = do
  let scripts = Map.fromList (Map.keys scriptsMap <&> (\script -> (scriptHash script, script)))
  case Map.lookup sh scripts of
    Just script -> 
      toCardanoScriptInAnyLang script <&> C.ReferenceScript C.ReferenceTxInsScriptsInlineDatumsInBabbageEra
    Nothing -> 
      pure C.ReferenceScriptNone

-- support only plutus v2
toCardanoScriptInAnyLang :: P.Script -> Either ToCardanoError C.ScriptInAnyLang
toCardanoScriptInAnyLang script =
  C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) . C.PlutusScript C.PlutusScriptV2
    <$> toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) script

fromCardanoTxOutV2 :: C.TxOut C.CtxTx era -> Either FromCardanoError PV2.TxOut
fromCardanoTxOutV2 (C.TxOut addr value datum refScript) =
    PV2.TxOut
    <$> Interop.fromCardanoAddressInEra addr
    <*> pure (fromCardanoTxOutValue value)
    <*> pure (Interop.fromCardanoTxOutDatum datum)
    <*> pure (refScriptToScriptHash refScript)

fromCardanoScriptHash :: C.ScriptHash -> P.ValidatorHash
fromCardanoScriptHash scriptHash = P.ValidatorHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes scriptHash

refScriptToScriptHash :: C.ReferenceScript era -> Maybe P.ScriptHash
refScriptToScriptHash C.ReferenceScriptNone = Nothing
refScriptToScriptHash (C.ReferenceScript _ (C.ScriptInAnyLang _ s)) =
    let (P.ValidatorHash h) = fromCardanoScriptHash $ C.hashScript s
     in Just $ P.ScriptHash h

fromCardanoTxOutValue :: C.TxOutValue era -> P.Value
fromCardanoTxOutValue (C.TxOutAdaOnly _ lovelace) = fromCardanoLovelace lovelace
fromCardanoTxOutValue (C.TxOutValue _ value)      = Interop.fromCardanoValue value

fromCardanoLovelace :: C.Lovelace -> P.Value
fromCardanoLovelace (C.lovelaceToQuantity -> C.Quantity lovelace) = lovelaceValueOf lovelace