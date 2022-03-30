module SubmitCli where

import RIO

import qualified CardanoTx.Models               as Sdk

import qualified Ledger.Ada as Ada
import Plutus.V1.Ledger.Ada    (lovelaceValueOf)

data SubmitCliService f =
  SubmitCliService
    { submit :: Sdk.TxCandidate -> f ()
    }

-- submit' :: Sdk.TxCandidate -> f ()
-- submit' Sdk.TxCandidate{..} = do
--     let
--         ins = txCandidateInputs

buildTx :: Sdk.TxCandidate -> IO ()
buildTx Sdk.TxCandidate{..} = do
  let
    ins = txCandidateInputs
    insForCli = fmap (\Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutRef}} -> "--tx-in" ++ (show fullTxOutRef) ++ "\"")
    outs = txCandidateOutputs
    outsCli = fmap (\out -> outToCli out) outs
    res = show insForCli ++ show outsCli
  print res

outToCli :: Sdk.Tx Candidate{txOutCandidateAddress, txOutCandidateValue} =
  let
    adaValue = Ada.fromValue txOutCandidateValue
    valueWOAda = flattenValue $ txOutCandidateValue <> (- Ada.lovelaceValueOf adaValue)
    resToken = fmap (\cs tn v -> "+" ++ v) valueWOAda
    resV = fmap (\cs tn v -> "\"" ++ v ++ " " ++ show cs ++ "." show tn ++ "\"")
  in txOutCandidateAddress ++ "+" ++ adaValue ++ resToken ++ resV
  