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
import           SubmitAPI.Internal.Transaction
import           NetworkAPI.Service             hiding (submitTx)
import qualified NetworkAPI.Service             as Network
import           NetworkAPI.Env
import           WalletAPI.Utxos
import           WalletAPI.Vault

data Transactions f era = Transactions
  { finalizeTx :: Sdk.TxCandidate -> f (C.Tx era)
  , submitTx   :: C.Tx era -> f ()
  }

mkTransactions
  :: (MonadThrow f, MonadIO f)
  => Network f
  -> WalletOutputs f
  -> Vault f
  -> TxAssemblyConfig
  -> Transactions f C.AlonzoEra
mkTransactions network utxos wallet conf = Transactions
  { finalizeTx = finalizeTx' network utxos wallet conf
  , submitTx   = Network.submitTx network
  }

finalizeTx'
  :: MonadThrow f
  => MonadIO f
  => Network f
  -> WalletOutputs f
  -> Vault f
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' Network{..} utxos Vault{..} conf@TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  sysenv      <- getSystemEnv
  collaterals <- selectCollaterals utxos sysenv conf txc

  (C.BalancedTxBody txb _ _) <- buildBalancedTx sysenv (getChangeAddr deafultChangeAddr) collaterals txc
  let
    allInputs   = (txCandidateInputs <&> Sdk.fullTxInTxOut) ++ (Set.elems collaterals <&> Sdk.fullCollateralTxInTxOut)
    signatories = allInputs >>= getPkh
      where
        getPkh Sdk.FullTxOut{fullTxOutAddress=P.Address (P.PubKeyCredential pkh) _} = [pkh]
        getPkh _                                                                    = []
  signers <- mapM (\pkh -> getSigningKey pkh >>= maybe (throwM $ SignerNotFound pkh) pure) signatories
  pure $ signTx txb signers

selectCollaterals
  :: MonadThrow f
  => MonadIO f
  => WalletOutputs f
  -> SystemEnv
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn)
selectCollaterals WalletOutputs{selectUtxosStrict} SystemEnv{..} TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  let isScriptIn Sdk.FullTxIn{fullTxInType=P.ConsumeScriptAddress {}} = True
      isScriptIn _                                                    = False

      scriptInputs = filter isScriptIn txCandidateInputs

      collectCollaterals knownCollaterals = do
        let
          estimateCollateral' collaterals = do
            fee <- estimateTxFee pparams network collaterals txc
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
