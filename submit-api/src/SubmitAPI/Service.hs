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
import           WalletAPI.Vault
import           Cardano.Crypto.DSIGN.SchnorrSecp256k1

data Transactions f era = Transactions
  { estimateTxFee :: Set.Set Sdk.FullCollateralTxIn -> Sdk.TxCandidate -> f C.Lovelace
  , finalizeTx    :: Sdk.TxCandidate -> f (C.Tx era)
  , submitTx      :: C.Tx era -> f C.TxId
  }

mkTransactions
  :: (MonadThrow f, MonadIO f)
  => CardanoNetwork f C.BabbageEra
  -> C.NetworkId
  -> Map P.Script C.TxIn
  -> WalletOutputs f
  -> Vault f
  -> TxAssemblyConfig
  -> Transactions f C.BabbageEra
mkTransactions network networkId refScriptsMap utxos wallet conf = Transactions
  { estimateTxFee = estimateTxFee' network networkId refScriptsMap
  , finalizeTx    = finalizeTx' network networkId refScriptsMap utxos wallet conf
  , submitTx      = submitTx' network
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
  -> Sdk.TxCandidate
  -> f (C.Tx C.BabbageEra)
finalizeTx' CardanoNetwork{..} network refScriptsMap utxos Vault{..} conf@TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  sysenv      <- getSystemEnv
  collaterals <- selectCollaterals utxos sysenv refScriptsMap network conf txc

  (C.BalancedTxBody txb _ _) <- Internal.buildBalancedTx sysenv refScriptsMap network (getChangeAddr deafultChangeAddr) collaterals txc
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
