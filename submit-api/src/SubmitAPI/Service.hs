module SubmitAPI.Service where

import           RIO
import qualified Data.Set              as Set
import qualified Data.ByteString.Char8 as B8
import           Data.Word             (Word64)

import qualified CardanoTx.Models           as Sdk
import           Cardano.Ledger             as C
import           Cardano.Api                as C
import qualified Ledger                     as P
import qualified Ledger.Ada                 as P
import qualified Plutus.Contract.CardanoAPI as Interop

import SubmitAPI.Config
import SubmitAPI.Internal.Transaction
import NetworkAPI.Service
import NetworkAPI.Env
import WalletAPI.Vault
import SubmitAPI.Config

data SubmitService f = SubmitService
  { finalizeTx :: Sdk.TxCandidate -> f (C.Tx C.AlonzoEra)
  , submitTx   :: C.Tx C.AlonzoEra -> f ()
  }

mkSubmitService :: TxAssemblyConfig -> SubmitService f
mkSubmitService TxAssemblyConfig{..} = undefined

finalizeTx'
  :: MonadThrow f
  => NetworkParams f
  -> Vault f
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (C.Tx C.AlonzoEra)
finalizeTx' network wallet conf@TxAssemblyConfig{..} txc@(Sdk.TxCandidate{..}) = undefined

mkCollaterals
  :: MonadThrow f
  -> Vault f
  -> SystemEnv
  -> TxAssemblyConfig
  -> Sdk.TxCandidate
  -> f (Set.Set Sdk.FullCollateralTxIn)
mkCollaterals SystemEnv{..} pparams wallet TxAssemblyConfig{..} txc@Sdk.TxCandidate{..} = do
  let isScriptIn Sdk.FullTxIn{fullTxInType=ConsumeScriptAddress _ _ _} = True
      isScriptIn _                                                     = False

      scriptInputs = filter isScriptIn (Set.elems txCandidateInputs)

      collectCollaterals knownCollaterals = do
        let estimateCollateral' collaterals = do
          (C.BalancedTxBody _ _ (C.Lovelace fee)) <- buildBalancedTx sysenv dummyAddress collaterals txc
            where dummyAddress = Sdk.ChangeAddress $ P.Address (B8.pack $ show (0 :: Word64))
          pure $ P.Lovelace $ collateralPercent * fee `div` 100

        collateral <- estimateCollateral' knownCollaterals
        utxos      <- selectUtxos wallet collateralValue >>= maybe (throwM FailedToSatisfyCollateral) pure
          where collateralValue = P.toValue collateral

        collateral' <- estimateCollateral' collaterals
          where collaterals = fmap FullCollateralTxIn utxos

        if collateral' > collateral
        then collectCollaterals collaterals
        else pure collaterals

  case (scriptInputs, collateralPolicy) of
    ([], _)    -> pure $ Lovelace 0
    (_, Cover) -> collectCollaterals mempty
    _          -> throwM CollateralNotAllowed