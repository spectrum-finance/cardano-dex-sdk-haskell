module SubmitAPI.Internal.Transaction where

import           RIO
import qualified Data.Text                 as T
import qualified Data.Map.Strict           as Map
import           Codec.Serialise
import           Data.ByteString.Lazy      (toStrict)
import           Data.Text.Prettyprint.Doc (Pretty(..))
import qualified Data.Set                  as Set

import           Cardano.Api                    hiding (TxBodyError)
import           Cardano.Api.Shelley            (ProtocolParameters)
import           Cardano.Slotting.Time          (SystemStart)
import qualified Ledger                         as P
import qualified Plutus.Contract.CardanoAPI     as Interop
import           Plutus.Contract.CardanoAPITemp as Interop
import qualified Ledger.Ada                     as Ada

import qualified CardanoTx.Models   as Sdk
import           CardanoTx.ToPlutus
import           NetworkAPI.Env

signTx
  :: TxBody AlonzoEra
  -> [ShelleyWitnessSigningKey]
  -> Tx AlonzoEra
signTx body keys =
  makeSignedTransaction wits body
    where wits = keys <&> makeShelleyKeyWitness body

buildBalancedTx
  :: MonadThrow f
  => SystemEnv
  -> Sdk.ChangeAddress
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f (BalancedTxBody AlonzoEra)
buildBalancedTx SystemEnv{..} defaultChangeAddr collateral txc@(Sdk.TxCandidate{..}) = do
  let era          = AlonzoEra
      eraInMode    = AlonzoEraInCardanoMode
      witOverrides = Nothing

  txBody     <- buildTxBodyContent pparams network collateral txc
  inputsMap  <- mkInputsUTxO network (Set.elems txCandidateInputs)
  changeAddr <- absorbError $ case txCandidateChangePolicy of
    Just (Sdk.ReturnTo addr) -> Interop.toCardanoAddress network addr
    _                        -> Interop.toCardanoAddress network $ Sdk.getAddress defaultChangeAddr

  absorbBalancingError $ makeTransactionBodyAutoBalance eraInMode sysstart eraHistory pparams pools inputsMap txBody changeAddr witOverrides
    where
      absorbBalancingError (Left e)  = throwM $ BalancingError $ T.pack $ show e
      absorbBalancingError (Right a) = pure a

buildTxBodyContent
  :: MonadThrow f
  => ProtocolParameters
  -> NetworkId
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f (TxBodyContent BuildTx AlonzoEra)
buildTxBodyContent protocolParams network collateral Sdk.TxCandidate{..} = do
  txIns           <- buildTxIns $ Set.elems txCandidateInputs
  txInsCollateral <- buildTxCollateral $ Set.elems collateral
  txOuts          <- buildTxOuts network txCandidateOutputs
  txFee           <- absorbError $ Interop.toCardanoFee dummyFee
  txValidityRange <- absorbError $ Interop.toCardanoValidityRange txCandidateValidRange
  txMintValue     <- absorbError $ Interop.toCardanoMintValue (Sdk.unMintValue txCandidateValueMint) txCandidateMintPolicies
  txData          <- collectInputsData $ Set.elems txCandidateInputs

  pure $ TxBodyContent
    { txIns             = txIns
    , txInsCollateral   = txInsCollateral
    , txOuts            = txOuts
    , txFee             = txFee
    , txValidityRange   = txValidityRange
    , txExtraScriptData = BuildTxWith $ Interop.toCardanoExtraScriptData (Map.elems txData)
    , txMintValue       = txMintValue
    , txProtocolParams  = BuildTxWith $ Just protocolParams
    , txScriptValidity  = TxScriptValidityNone
    -- unused:
    , txMetadata       = TxMetadataNone
    , txAuxScripts     = TxAuxScriptsNone
    , txExtraKeyWits   = TxExtraKeyWitnessesNone
    , txWithdrawals    = TxWithdrawalsNone
    , txCertificates   = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    }

buildTxIns
  :: MonadThrow f
  => [Sdk.FullTxIn]
  -> f [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn AlonzoEra))]
buildTxIns =
    mapM translate
  where
    translate Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{..}, ..} = do
      sWit <- absorbError $ Interop.toCardanoTxInWitness fullTxInType
      txIn <- absorbError $ Interop.toCardanoTxIn fullTxOutRef

      pure (txIn, BuildTxWith sWit)

buildTxCollateral
  :: MonadThrow f
  => [Sdk.FullCollateralTxIn]
  -> f (TxInsCollateral AlonzoEra)
buildTxCollateral ins =
    TxInsCollateral CollateralInAlonzoEra <$> mapM translate ins
  where
    translate Sdk.FullCollateralTxIn{fullCollateralTxInTxOut=Sdk.FullTxOut{..}} =
      absorbError $ Interop.toCardanoTxIn fullTxOutRef

buildTxOuts
  :: MonadThrow f
  => NetworkId
  -> [Sdk.TxOutCandidate]
  -> f [TxOut AlonzoEra]
buildTxOuts network =
    mapM translate
  where
    translate sdkOut = absorbError $ Interop.toCardanoTxOut network $ toPlutus sdkOut

mkInputsUTxO
  :: MonadThrow f
  => NetworkId
  -> [Sdk.FullTxIn]
  -> f (UTxO AlonzoEra)
mkInputsUTxO network inputs =
    mapM (absorbError . translate) inputs <&> UTxO . Map.fromList
  where
    translate Sdk.FullTxIn{fullTxInTxOut=out@Sdk.FullTxOut{..}, ..} = do
      txIn  <- Interop.toCardanoTxIn fullTxOutRef
      txOut <- Interop.toCardanoTxOut network $ toPlutus out

      pure (txIn, txOut)

collectInputsData :: MonadThrow f => [Sdk.FullTxIn] -> f (Map.Map P.DatumHash P.Datum)
collectInputsData inputs = do
  rawData <- mapM extractInputDatum inputs
  pure $ Map.fromList $ rawData >>= (maybe mempty pure)

extractInputDatum :: MonadThrow f => Sdk.FullTxIn -> f (Maybe (P.DatumHash, P.Datum))
extractInputDatum Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutDatumHash=Just dh, fullTxOutDatum=Just d}} =
  pure $ Just (dh, d)
extractInputDatum Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutDatumHash=Just dh}} =
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
  | UnresolvedData P.DatumHash
  | BalancingError Text
  | BalancingError Text
  | CollateralNotAllowed
  | FailedToSatisfyCollateral
  deriving (Show, Exception)

absorbError :: MonadThrow f => Either Interop.ToCardanoError a -> f a
absorbError (Left err) = throwM $ adaptInteropError err
absorbError (Right vl) = pure vl

adaptInteropError :: Interop.ToCardanoError -> TxAssemblyError
adaptInteropError err =
    case err of
      Interop.EvaluationError _                  -> EvaluationError renderedErr
      Interop.TxBodyError _                      -> TxBodyError renderedErr
      Interop.DeserialisationError               -> DeserializationError
      Interop.InvalidValidityRange               -> InvalidValidityRange
      Interop.ValueNotPureAda                    -> ValueNotPureAda
      Interop.OutputHasZeroAda                   -> OutputHasZeroAda
      Interop.StakingPointersNotSupported        -> StakingPointersNotSupported
      Interop.SimpleScriptsNotSupportedToCardano -> SimpleScriptsNotSupportedToCardano
      Interop.MissingTxInType                    -> MissingTxInType
      Interop.Tag _ e                            -> adaptInteropError e
  where renderedErr = T.pack $ show $ pretty err

serializePlutusScript :: P.Script -> SerializedScript
serializePlutusScript s = toShort $ toStrict $ serialise $ s

type SerializedScript = ShortByteString
