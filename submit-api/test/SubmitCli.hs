module SubmitCli where

import RIO

import qualified CardanoTx.Models               as Sdk

import qualified Ledger.Ada as Ada
import Plutus.V1.Ledger.Ada    (lovelaceValueOf)
import Plutus.V1.Ledger.Value
import           Data.Functor    ((<&>))
import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

import Gen.CardanoTx
import Spec.Network

import qualified Cardano.Api as C

import NetworkAPI.Env
import SubmitAPI.Internal.Transaction
import CardanoTx.Models
import CardanoTx.Interop as Interop
import Plutus.V1.Ledger.Tx
import qualified Cardano.Api                as C
import qualified Data.Text as T
import Cardano.Api
import PlutusTx.Builtins.Internal 

import qualified Plutus.V1.Ledger.Address as PAddress
import qualified Plutus.V1.Ledger.Credential as PCredential
import qualified Plutus.V1.Ledger.Crypto as PCrypto
import qualified Cardano.Api as CAddress

data SubmitCliService f =
  SubmitCliService
    { submit :: Sdk.TxCandidate -> f ()
    }

-- submit' :: Sdk.TxCandidate -> f ()
-- submit' Sdk.TxCandidate{..} = do
--     let
--         ins = txCandidateInputs

buildTx :: Sdk.TxCandidate -> String
buildTx Sdk.TxCandidate{..} =
  let
    ins = txCandidateInputs
    insForCli = foldl (\acc Sdk.FullTxIn{fullTxInTxOut=Sdk.FullTxOut{fullTxOutRef}} -> acc ++ " --tx-in " ++ (prittyTxOutRef fullTxOutRef)) mempty txCandidateInputs
    outs = txCandidateOutputs
    outsCli = foldl (\acc out -> acc ++ outToCli out)  mempty outs
    res = show insForCli ++ " " ++ show outsCli
  in "$CARDANO_CLI transaction build" ++ res

outToCli :: Sdk.TxOutCandidate -> String
outToCli candidate =
  let
    value = Sdk.txOutCandidateValue candidate
    address = Sdk.txOutCandidateAddress candidate
    adaValue = Ada.getLovelace . Ada.fromValue $  value
    valueWOAda = flattenValue $ value <> ( Ada.lovelaceValueOf adaValue)
    resToken = foldl (\acc (cs, tn, v) -> acc ++ "+" ++ (show v)) mempty valueWOAda
    resV = foldl (\acc (cs, tn, i) -> acc ++ (show i) ++ " " ++ show cs ++ "." ++ show tn) "" valueWOAda
    
    -- a = C.makeShelleyAddress Mainnet
    -- addr = T.unpack $ C.serialiseAddress $ address
    --  ++ (addr)
  in " --tx-out " ++ "+" ++ (show adaValue) ++ (show resToken) ++ (show resV)

prittyTxOutRef :: TxOutRef -> String
prittyTxOutRef TxOutRef{..} =
  show txOutRefId ++ "#" ++ (show txOutRefIdx)

addrInterop :: PAddress.Address -> ()
addrInterop PAddress.Address{addressCredential, addressStakingCredential} =
  let
    addressCredentialInterop = case addressCredential of
      PCredential.PubKeyCredential hash ->
        let
          BuiltinByteString bs = PCrypto.getPubKeyHash hash
          deser = CAddress.deserialiseFromRawBytes bs
        in
          CAddress.PaymentCredentialByKey hash
      PCredential.ScriptCredential hash -> CAddress.PaymentCredentialByScript hash
  in ()
  