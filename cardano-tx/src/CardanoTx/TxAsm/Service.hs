module CardanoTx.TxAsm.Service where

import qualified Data.Set            as Set
import qualified Data.Map            as Map
import           Control.Monad.Error

import Ledger                      (Address(..), Datum, DatumHash)
import Ledger.Scripts              (datumHash)
import Plutus.Contract.Wallet
import Plutus.V1.Ledger.Credential (Credential(..))
import Plutus.V1.Ledger.Tx         as P
import CardanoTx.TxAsm.Config
import CardanoTx.Models

-- signTx :: TxId -> Crypto.XPrv -> Signature (import Ledger.Crypto)
-- Use Plutus.Contract.Wallet ?

data TxAsmError = UnresolvedData TxOutRef

data TxAsm f = TxAsm
  { assembleTx :: TxCandidate -> f P.Tx
  }

assembleTx' :: (MonadError TxAsmError f) => AssemblyConfig -> TxCandidate -> f P.Tx
assembleTx' AssemblyConfig{..} TxCandidate{..} = do
  let inputs'  = Set.elems txCandidateInputs
      pInputs  = fmap mkPlutusInput inputs'
      pOutputs = fmap mkPlutusOutput txCandidateOutputs

  let outData = collectOutputsData txCandidateOutputs

  pure $ P.Tx
    { txInputs      = Set.fromList pInputs
    , txCollateral  = mempty -- todo: should we set it ourselves?
    , txOutputs     = pOutputs
    , txMint        = unMintValue txCandidateValueMint
    , txFee         = mempty
    , txValidRange  = txCandidateValidRange
    , txMintScripts = txCandidateMintPolicies
    , txSignatures  = mempty
    , txRedeemers   = undefined
    , txData        = outData
    }

collectOutputsData :: [TxOutCandidate] -> Map.Map DatumHash Datum
collectOutputsData outputs =
    Map.fromList $ outputs >>= tryGetDatum
  where
    tryGetDatum TxOutCandidate{txOutCandidateDatum=Just d} = pure (datumHash d, d)
    tryGetDatum _                                          = mempty

collectInputsData :: MonadError TxAsmError f => [FullTxIn] -> f (Map.Map DatumHash Datum)
collectInputsData inputs = do
  rawData <- mapM extractInputDatum inputs
  pure $ Map.fromList $ rawData >>= (maybe mempty pure)

extractInputDatum :: MonadError TxAsmError f => FullTxIn -> f (Maybe (DatumHash, Datum))
extractInputDatum FullTxIn{fullTxInTxOut=FullTxOut{fullTxOutDatumHash=Just dh, fullTxOutDatum=Just d}} =
  pure $ Just (dh, d)
extractInputDatum FullTxIn{fullTxInTxOut=FullTxOut{fullTxOutDatumHash=Just _, ..}} =
  throwError $ UnresolvedData fullTxOutRef
extractInputDatum _ = pure Nothing

mkPlutusInput ::  FullTxIn -> P.TxIn
mkPlutusInput FullTxIn{fullTxInTxOut=FullTxOut{..}, ..} =
    P.TxIn fullTxOutRef tpe
  where
    tpe =
      Just $ case (fullTxOutAddress, fullTxInScript, fullTxInRedeemer, fullTxOutDatum) of
        (Address (ScriptCredential _) _, Just s, Just r, Just d) -> ConsumeScriptAddress s r d
        (Address (ScriptCredential _) _, _, _, _)                -> ConsumeSimpleScriptAddress
        (Address (PubKeyCredential _) _, _, _, _)                -> ConsumePublicKeyAddress

mkPlutusOutput :: TxOutCandidate -> P.TxOut
mkPlutusOutput TxOutCandidate{..} =
  P.TxOut txOutCandidateAddress txOutCandidateValue dh
    where dh = fmap datumHash txOutCandidateDatum
