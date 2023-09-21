module SubmitAPI.Service where

import           RIO
import qualified Data.Set    as Set
import           GHC.Natural (naturalToInteger)

import qualified Cardano.Api                 as C
import qualified Cardano.Api.Shelley         as C
import qualified Ledger                      as P
import qualified Ledger.Ada                  as P
import qualified Plutus.V1.Ledger.Credential as P

import qualified CardanoTx.Models               as Sdk
import           SubmitAPI.Config
import qualified SubmitAPI.Internal.Transaction  as Internal
import           SubmitAPI.Internal.Transaction  (TxAssemblyError(..))
import           NetworkAPI.Service  hiding (submitTx)
import qualified NetworkAPI.Service  as Network
import           NetworkAPI.Types
import           WalletAPI.Utxos
import           NetworkAPI.HttpService hiding (submitTx, submitTx')
import qualified NetworkAPI.HttpService as HttpApi
import           WalletAPI.Vault
import           Cardano.Crypto.DSIGN.SchnorrSecp256k1
import Cardano.Api (Lovelace(Lovelace))
import Plutus.V1.Ledger.Value (assetClass)
import Plutus.V1.Ledger.Api (adaSymbol)
import Plutus.V1.Ledger.Api (adaToken)
import Ledger.Value (assetClassValueOf)
import System.Logging.Hlog
import Cardano.Api.SerialiseTextEnvelope

data Transactions f era = Transactions
  { estimateTxFee :: Set.Set Sdk.FullCollateralTxIn -> Sdk.TxCandidate -> f C.Lovelace
  , finalizeTx    :: Sdk.TxCandidate -> f (C.Tx era)
  , finalizeTxWithExplFeePolicy :: FeePolicy -> Sdk.TxCandidate -> f (C.Tx era)
  , finalizeTxUnsafe :: Sdk.TxCandidate -> Integer -> f (C.Tx era)
  , submitTx         :: C.Tx era -> f C.TxId
  , submitTxByHttp   :: C.Tx era -> f C.TxId
  }

mkTransactions
  :: (MonadThrow f, MonadIO f)
  => UnsafeEvalConfig
  -> Logging f
  -> CardanoNetwork f C.BabbageEra
  -> CardanoHttpNetwork f C.BabbageEra
  -> C.NetworkId
  -> Map P.Script C.TxIn
  -> WalletOutputs f
  -> Vault f
  -> TxAssemblyConfig
  -> Transactions f C.BabbageEra
mkTransactions cfg logging network httpNetwork networkId refScriptsMap utxos wallet conf = Transactions
  { estimateTxFee    = estimateTxFee' network networkId refScriptsMap
  , finalizeTx       = finalizeTx' network networkId refScriptsMap utxos wallet conf (feePolicy conf)
  , finalizeTxWithExplFeePolicy = finalizeTx' network networkId refScriptsMap utxos wallet conf
  , finalizeTxUnsafe = finalizeTxUnsafe' cfg logging network networkId refScriptsMap utxos wallet conf
  , submitTx         = submitTx' network
  , submitTxByHttp   = HttpApi.submitTx httpNetwork
  }

estimateTxFee'
  :: MonadThrow f
  => MonadIO f
  => CardanoNetwork f C.BabbageEra
  -> C.NetworkId
  -> Map P.Script C.TxIn
  -> Set.Set Sdk.FullCollateralTxIn
  -> Sdk.TxCandidate
  -> f C.Lovelace
estimateTxFee' CardanoNetwork{..} network refScriptsMap collateral txc = do
  SystemEnv{pparams} <- getSystemEnv
  Internal.estimateTxFee pparams network refScriptsMap collateral txc

finalizeTx'
  :: MonadThrow f
  => CardanoNetwork f C.BabbageEra
  -> C.NetworkId
  -> Map P.Script C.TxIn
  -> WalletOutputs f
  -> Vault f
  -> TxAssemblyConfig
  -> FeePolicy
  -> Sdk.TxCandidate
  -> f (C.Tx C.BabbageEra)
finalizeTx' CardanoNetwork{..} network refScriptsMap utxos Vault{..} conf@TxAssemblyConfig{..} feeP txc@Sdk.TxCandidate{..} = do
  sysenv      <- getSystemEnv
  collaterals <- selectCollaterals utxos sysenv refScriptsMap network conf txc

  (C.BalancedTxBody txb _ _) <- Internal.buildBalancedTx sysenv refScriptsMap network (getChangeAddr deafultChangeAddr) collaterals txc feeP
  let
    allInputs   = (Set.elems txCandidateInputs <&> Sdk.fullTxInTxOut) ++ (Set.elems collaterals <&> Sdk.fullCollateralTxInTxOut)
    signatories = allInputs >>= getPkh
      where
        getPkh Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _} = [pkh]
        getPkh _                                                                    = []
  signers <- mapM (\pkh -> getSigningKey pkh >>= maybe (throwM $ SignerNotFound pkh) pure) signatories
  pure $ Internal.signTx txb signers

finalizeTxUnsafe'
  :: MonadThrow f
  => UnsafeEvalConfig
  -> Logging f
  -> CardanoNetwork f C.BabbageEra
  -> C.NetworkId
  -> Map P.Script C.TxIn
  -> WalletOutputs f
  -> Vault f
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> Integer
  -> f (C.Tx C.BabbageEra)
finalizeTxUnsafe' cfg Logging{..} CardanoNetwork{..} network refScriptsMap utxos Vault{..} conf@TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} changeValue = do
  sysenv                   <- getSystemEnv
  (collaterals, colAmount) <- selectCollateralsUnsafe utxos sysenv conf txc
  (C.BalancedTxBody txb _ _) <- Internal.buildBalancedTxUnsafe cfg sysenv refScriptsMap network (getChangeAddr deafultChangeAddr) collaterals txc changeValue colAmount
  let
    allInputs   = (Set.elems txCandidateInputs <&> Sdk.fullTxInTxOut) ++ (Set.elems collaterals <&> Sdk.fullCollateralTxInTxOut)
    signatories = allInputs >>= getPkh
      where
        getPkh Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _} = [pkh]
        getPkh _                                                                    = []
  signers <- mapM (\pkh -> getSigningKey pkh >>= maybe (throwM $ SignerNotFound pkh) pure) signatories
  pure $ Internal.signTx txb signers

submitTx' :: Monad f => CardanoNetwork f C.BabbageEra -> C.Tx C.BabbageEra -> f C.TxId
submitTx' CardanoNetwork{submitTx} tx = do
  let test = textEnvelopeToJSON Nothing tx
  submitTx tx
  pure . C.getTxId . C.getTxBody $ tx

selectCollaterals
  :: MonadThrow f
  => WalletOutputs f
  -> SystemEnv
  -> Map P.Script C.TxIn
  -> C.NetworkId
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn)
selectCollaterals WalletOutputs{selectUtxosStrict} SystemEnv{..} refScriptsMap network TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  let isScriptIn Sdk.FullTxIn{fullTxInType=P.ConsumeScriptAddress {}} = True
      isScriptIn _                                                    = False

      scriptInputs = filter isScriptIn (Set.elems txCandidateInputs)

      collectCollaterals knownCollaterals = do
        let
          estimateCollateral' collaterals = do
            fee <- Internal.estimateTxFee pparams network refScriptsMap collaterals txc
            let (C.Quantity fee') = C.lovelaceToQuantity fee
                collateralPercent = naturalToInteger $ fromMaybe 0 (C.protocolParamCollateralPercent pparams)
            pure $ P.Lovelace $ collateralPercent * fee' `div` 100

        collateral <- estimateCollateral' knownCollaterals
        utxos      <- selectUtxosStrict (P.toValue collateral) >>= maybe (throwM FailedToSatisfyCollateral) pure

        let collaterals = Set.fromList $ Set.elems utxos <&> Sdk.FullCollateralTxIn

        collateral' <- estimateCollateral' collaterals

        if collateral' > collateral
          then collectCollaterals collaterals
          else pure collaterals

  case (scriptInputs, collateralPolicy) of
    ([], _)    -> pure mempty
    (_, Cover) -> collectCollaterals mempty
    _          -> throwM CollateralNotAllowed

selectCollateralsUnsafe
  :: MonadThrow f
  => WalletOutputs f
  -> SystemEnv
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn, Integer)
selectCollateralsUnsafe WalletOutputs{selectUtxosStrict} SystemEnv{..} TxAssemblyConfig{..} Sdk.TxCandidate{..} = do
  let 
      collectCollaterals = do
        utxos      <- selectUtxosStrict (P.toValue (P.Lovelace 1300000)) >>= maybe (throwM FailedToSatisfyCollateral) pure
        let 
          collaterals = Set.fromList $ Set.elems utxos <&> Sdk.FullCollateralTxIn
          adaAC = assetClass adaSymbol adaToken
          origValue = foldl (\acc Sdk.FullTxOut{..} -> acc + assetClassValueOf fullTxOutValue adaAC) 0 utxos

        pure (collaterals, origValue)

  case collateralPolicy of
    Cover -> collectCollaterals
    _     -> pure (mempty, 0)
