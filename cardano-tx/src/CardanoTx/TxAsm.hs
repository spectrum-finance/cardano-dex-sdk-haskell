module CardanoTx.TxAsm where

import Ledger                      (Address(..))
import Plutus.Contract.Wallet
import Plutus.V1.Ledger.Credential (Credential(..))
import Plutus.V1.Ledger.Tx         as P
import CardanoTx.Models

data TxAsm f = TxAsm
  { assembleTx :: TxCandidate -> f P.Tx
  }

assembleTx' :: TxCandidate -> f P.Tx
assembleTx' TxCandidate{..} = undefined

mkPlutusInput :: FullTxIn -> P.TxIn
mkPlutusInput FullTxIn{fullTxInTxOut=FullTxOut{..}, ..} =
  P.TxIn fullTxOutRef tpe
  where
    tpe =
      Just $ case (fullTxOutAddress, fullTxInScript, fullTxInRedeemer, fullTxOutDatum) of
        (Address (ScriptCredential _) _, Just s, Just r, Just d) -> ConsumeScriptAddress s r d
        (Address (ScriptCredential _) _, _, _, _)                -> ConsumeSimpleScriptAddress
        (Address (PubKeyCredential _) _, _, _, _)                -> ConsumePublicKeyAddress
