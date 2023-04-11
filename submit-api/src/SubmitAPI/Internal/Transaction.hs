{-# OPTIONS_GHC -Wno-unused-imports #-}
module SubmitAPI.Internal.Transaction where

import           RIO
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map

import           Codec.Serialise           (serialise)
import           Data.ByteString.Lazy      (toStrict)
import           Prettyprinter             (Pretty(..))
import qualified Data.Set                  as Set
import qualified RIO.List                  as List

import           Cardano.Api          hiding (TxBodyError)
import           Cardano.Api.Shelley  (ProtocolParameters(..), ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra))
import qualified Cardano.Api.Shelley            as C
import           Plutus.Script.Utils.V1.Scripts
import qualified Plutus.V2.Ledger.Tx            as PV2
import           Plutus.V2.Ledger.Api           (Datum(Datum))
import qualified Ledger                         as P
import           Ledger                         (ToCardanoError(Tag))
import qualified Ledger.Tx.CardanoAPI           as Interop
import qualified CardanoTx.Interop              as Interop
import qualified Ledger.Ada                     as Ada
import           Plutus.Script.Utils.Scripts    (datumHash)

import qualified CardanoTx.Models             as Sdk
import qualified SubmitAPI.Internal.Balancing as Balancing
import           CardanoTx.ToPlutus
import           NetworkAPI.Types

signTx
  :: TxBody BabbageEra
  -> [ShelleyWitnessSigningKey]
  -> Tx BabbageEra
signTx body keys =
  makeSignedTransaction wits body
    where wits = keys <&> makeShelleyKeyWitness body

buildBalancedTx
  :: (MonadThrow f)
  => SystemEnv
  -> Map P.Script C.TxIn
  -> NetworkId
  -> Sdk.ChangeAddress
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f (BalancedTxBody BabbageEra)
buildBalancedTx SystemEnv{..} refScriptsMap network defaultChangeAddr collateral txc@Sdk.TxCandidate{..} = do
  let eraInMode    = BabbageEraInCardanoMode
      witOverrides = Nothing
  txBody     <- buildTxBodyContent pparams network refScriptsMap collateral txc
  inputsMap  <- buildInputsUTxO network refScriptsMap (Set.elems txCandidateInputs) txCandidateRefIns
  changeAddr <- absorbError $ case txCandidateChangePolicy of
    Just (Sdk.ReturnTo addr) -> Interop.toCardanoAddressInEra network addr
    _                        -> Interop.toCardanoAddressInEra network $ Sdk.getAddress defaultChangeAddr
  absorbBalancingError $
    Balancing.makeTransactionBodyAutoBalance eraInMode sysstart eraHistory pparams pools inputsMap txBody changeAddr witOverrides
      where
        absorbBalancingError (Left e)  = throwM $ BalancingError $ T.pack $ displayError e
        absorbBalancingError (Right a) = pure a

estimateTxFee
  :: (MonadThrow f)
  => ProtocolParameters
  -> NetworkId
  -> Map P.Script C.TxIn
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f Lovelace
estimateTxFee pparams network refScriptsMap collateral txc = do
  txBodyContent <- buildTxBodyContent pparams network refScriptsMap collateral txc
  txBody        <- either (throwM . TxBodyError . T.pack . show) pure (makeTransactionBody txBodyContent)
  pure $ evaluateTransactionFee pparams txBody 0 0

buildTxBodyContent
  :: (MonadThrow f)
  => ProtocolParameters
  -> NetworkId
  -> Map P.Script C.TxIn
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f (TxBodyContent BuildTx BabbageEra)
buildTxBodyContent protocolParams network refScriptsMap collateral Sdk.TxCandidate{..} = do
  txIns           <- buildTxIns refScriptsMap $ Set.elems txCandidateInputs
  txInsRef        <- buildTxRefIns txCandidateRefIns
  txInsCollateral <- buildTxCollateral $ Set.elems collateral
  txOuts          <- buildTxOuts network refScriptsMap txCandidateOutputs
  txFee           <- absorbError $ Interop.toCardanoFee dummyFee
  txValidityRange <- absorbError $ Interop.toCardanoValidityRange txCandidateValidRange
  txMintValue     <-
    let redeemers = buildMintRedeemers txCandidateMintInputs
        valueMint = Sdk.unMintValue txCandidateValueMint
        policies  = Map.fromList $ toList (Sdk.mintInputsPolicies txCandidateMintInputs) <&> (\policy -> (mintingPolicyHash policy, policy))
    in absorbError $ Interop.toCardanoMintValue redeemers valueMint policies
  wits <- absorbError $ traverse Interop.toCardanoPaymentKeyHash txCandidateSigners
  let
    wits' =
      if null wits
        then TxExtraKeyWitnessesNone
        else TxExtraKeyWitnesses ExtraKeyWitnessesInBabbageEra wits
  pure $ TxBodyContent
    { txIns            = txIns
    , txInsCollateral  = txInsCollateral
    , txInsReference   = txInsRef
    , txOuts           = txOuts
    , txTotalCollateral = TxTotalCollateralNone
    , txReturnCollateral = TxReturnCollateralNone
    , txFee            = txFee
    , txValidityRange  = txValidityRange
    , txMintValue      = txMintValue
    , txProtocolParams = BuildTxWith $ Just protocolParams
    , txExtraKeyWits   = wits'
    -- unused:
    , txScriptValidity = TxScriptValidityNone
    , txMetadata       = TxMetadataNone
    , txAuxScripts     = TxAuxScriptsNone
    , txWithdrawals    = TxWithdrawalsNone
    , txCertificates   = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    }

buildTxIns
  :: (MonadThrow f)
  => Map P.Script C.TxIn
  -> [Sdk.FullTxIn]
  -> f [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn BabbageEra))]
buildTxIns refScripstMap =
    mapM translate
  where
    translate Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{..}, ..} = do
      sWit <- absorbError $ Interop.toCardanoTxInWitnessV2 refScripstMap fullTxInType
      txIn <- absorbError $ Interop.toCardanoTxIn fullTxOutRef

      pure (txIn, BuildTxWith sWit)

buildTxRefIns
  :: (MonadThrow f)
  => [Sdk.FullTxOut ]
  -> f (TxInsReference BuildTx BabbageEra)
buildTxRefIns ins =
    TxInsReference ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> mapM translate ins
  where
    translate Sdk.FullTxOut{..} = do
      absorbError $ Interop.toCardanoTxIn fullTxOutRef

buildTxCollateral
  :: (MonadThrow f)
  => [Sdk.FullCollateralTxIn]
  -> f (TxInsCollateral BabbageEra)
buildTxCollateral ins =
    TxInsCollateral CollateralInBabbageEra <$> mapM translate ins
  where
    translate Sdk.FullCollateralTxIn{fullCollateralTxInTxOut=Sdk.FullTxOut{..}} =
      absorbError $ Interop.toCardanoTxIn fullTxOutRef

buildTxOuts
  :: (MonadThrow f)
  => NetworkId
  -> Map P.Script C.TxIn
  -> [Sdk.TxOutCandidate]
  -> f [TxOut CtxTx BabbageEra]
buildTxOuts network scriptsMap =
    mapM translate
  where
    datumCast :: (PV2.OutputDatum -> Either ToCardanoError (TxOutDatum CtxTx BabbageEra))
    datumCast mDh =
      case mDh of
        PV2.NoOutputDatum -> pure TxOutDatumNone
        PV2.OutputDatumHash dh -> do
          scDataHash <- Interop.toCardanoScriptDataHash dh
          pure $ TxOutDatumHash ScriptDataInBabbageEra scDataHash
        PV2.OutputDatum d -> do
          pure $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (Interop.toCardanoScriptData (P.getDatum d))

    translate sdkOut = absorbError $ Interop.toCardanoTxOutV2 network scriptsMap datumCast $ toPlutus sdkOut

buildInputsUTxO
  :: (MonadThrow f)
  => NetworkId
  -> Map P.Script C.TxIn
  -> [Sdk.FullTxIn]
  -> [Sdk.FullTxOut]
  -> f (UTxO BabbageEra)
buildInputsUTxO network scriptsMap inputs refOutputs = do
  translatedInputs     <- mapM (absorbError . translate) inputs
  translatedRefOutputs <- mapM (absorbError . translateOutputs) refOutputs
  pure . UTxO . Map.fromList $ translatedInputs ++ translatedRefOutputs
  where
    datumCast :: (PV2.OutputDatum -> Either ToCardanoError (TxOutDatum CtxTx BabbageEra))
    datumCast mDh =
      case mDh of
        PV2.NoOutputDatum -> pure TxOutDatumNone
        PV2.OutputDatumHash dh -> do
          scDataHash <- Interop.toCardanoScriptDataHash dh
          pure $ TxOutDatumHash ScriptDataInBabbageEra scDataHash
        PV2.OutputDatum d -> do
          pure $ TxOutDatumInline ReferenceTxInsScriptsInlineDatumsInBabbageEra (Interop.toCardanoScriptData (P.getDatum d))

    translate Sdk.FullTxIn{fullTxInTxOut=out@Sdk.FullTxOut{..}} = do
      txIn  <- Interop.toCardanoTxIn fullTxOutRef
      txOut <- Interop.toCardanoTxOutV2 network scriptsMap datumCast $ toPlutus out
      pure (txIn, toCtxUTxOTxOut txOut)

    translateOutputs out@Sdk.FullTxOut{..} = do
      txIn  <- Interop.toCardanoTxIn fullTxOutRef
      txOut <- Interop.toCardanoTxOutV2 network scriptsMap datumCast $ toPlutus out
      pure (txIn, toCtxUTxOTxOut txOut)

buildMintRedeemers :: Sdk.MintInputs -> P.Redeemers
buildMintRedeemers Sdk.MintInputs{..} = Map.fromList $ Map.toList mintInputsRedeemers <&> first (P.RedeemerPtr P.Mint)

collectInputsData :: MonadThrow f => [Sdk.FullTxIn] -> f (Map.Map P.DatumHash P.Datum)
collectInputsData inputs = do
  rawData <- mapM extractInputDatum inputs
  pure $ Map.fromList $ rawData >>= maybe mempty pure

extractInputDatum :: MonadThrow f => Sdk.FullTxIn -> f (Maybe (P.DatumHash, P.Datum))
extractInputDatum Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutDatum=Sdk.KnownDatum d}} =
  pure $ Just (datumHash d, d)
extractInputDatum Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutDatum=Sdk.KnownDatumHash dh}} =
  throwM $ UnresolvedData dh
extractInputDatum _ = pure Nothing

dummyFee :: P.Value
dummyFee = Ada.lovelaceValueOf 0

data TxAssemblyError
  = EvaluationError Text
  | TxBodyError Text
  | DeserializationError
  | InvalidValidityRange
  | ValueNotPureAda
  | OutputHasZeroAda
  | StakingPointersNotSupported
  | SimpleScriptsNotSupportedToCardano
  | MissingTxInType
  | MissingMintingPolicy
  | MissingMintingPolicyRedeemer
  | ScriptPurposeNotSupported P.ScriptTag
  | PublicKeyInputsNotSupported
  | UnresolvedData P.DatumHash
  | BalancingError Text
  | CollateralNotAllowed
  | FailedToSatisfyCollateral
  | SignerNotFound P.PubKeyHash
  deriving (Show, Exception)

absorbError :: (MonadThrow f) => Either Interop.ToCardanoError a -> f a
absorbError (Left err) = throwM $ adaptInteropError err
absorbError (Right vl) = pure vl

adaptInteropError :: Interop.ToCardanoError -> TxAssemblyError
adaptInteropError err =
    case err of
      Interop.TxBodyError _                      -> TxBodyError renderedErr
      Interop.DeserialisationError               -> DeserializationError
      Interop.InvalidValidityRange               -> InvalidValidityRange
      Interop.ValueNotPureAda                    -> ValueNotPureAda
      Interop.OutputHasZeroAda                   -> OutputHasZeroAda
      Interop.StakingPointersNotSupported        -> StakingPointersNotSupported
      Interop.SimpleScriptsNotSupportedToCardano -> SimpleScriptsNotSupportedToCardano
      Interop.MissingTxInType                    -> MissingTxInType
      Interop.MissingMintingPolicy               -> MissingMintingPolicy
      Interop.MissingMintingPolicyRedeemer       -> MissingMintingPolicyRedeemer
      Interop.ScriptPurposeNotSupported t        -> ScriptPurposeNotSupported t
      Interop.Tag _ e                            -> adaptInteropError e
  where renderedErr = T.pack $ show $ pretty err

serializePlutusScript :: P.Script -> SerializedScript
serializePlutusScript s = toShort $ toStrict $ serialise s

type SerializedScript = ShortByteString
