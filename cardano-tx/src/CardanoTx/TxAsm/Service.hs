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

  extractedData <- mapM extractDatum inputs'
  let flattenedData = extractedData >>= (maybe mempty pure)

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
    , txData        = Map.fromList flattenedData
    }

extractDatum :: (MonadError TxAsmError f) => FullTxIn -> f (Maybe (DatumHash, Datum))
extractDatum FullTxIn{fullTxInTxOut=FullTxOut{fullTxOutDatumHash=Just dh, fullTxOutDatum=Just d}} =
  pure $ Just (dh, d)
extractDatum FullTxIn{fullTxInTxOut=FullTxOut{fullTxOutDatumHash=Just dh, ..}} =
  throwError $ UnresolvedData fullTxOutRef
extractDatum _ = pure Nothing

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
